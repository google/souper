// Copyright 2019 The Souper Authors. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#define DEBUG_TYPE "souper"

#include "llvm/ADT/APInt.h"
#include "llvm/Support/CommandLine.h"
#include "souper/Infer/ConstantSynthesis.h"
#include "souper/Infer/Interpreter.h"
#include "souper/Infer/Pruning.h"
#include "souper/Infer/Z3Driver.h"

extern unsigned DebugLevel;

namespace {
  using namespace llvm;
  static cl::opt<bool> EnableConcreteInterpreter("souper-constant-synthesis-use-concrete-interpreter",
    cl::desc("Use concrete interpreter in constant synthesis (default=false)"),
    cl::init(false));
  static cl::opt<unsigned> MaxSpecializations("souper-constant-synthesis-max-num-specializations",
    cl::desc("Maximum number of input specializations in constant synthesis (default=15)."),
    cl::init(15));
}

namespace souper {

Inst *getUBConstraint(Inst::Kind K, unsigned OpNum, Inst *C,
                      InstContext &IC) {
  switch (K) {

  case Inst::Shl:
  case Inst::LShr:
  case Inst::AShr:
    // right operand has to be < Width
    return (OpNum == 0) ?
      IC.getConst(llvm::APInt(1, true)) : 
      IC.getInst(Inst::Ult, 1, { C, IC.getConst(llvm::APInt(C->Width, C->Width)) });

  case Inst::UDiv:
  case Inst::SDiv:
  case Inst::SRem:
  case Inst::URem:
    // right operand can't be 0
    return (OpNum == 0) ?
      IC.getConst(llvm::APInt(1, true)) :
      IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C });

  default:
    return IC.getConst(llvm::APInt(1, true));
  }
}

Inst *getConstConstraint(Inst::Kind K, unsigned OpNum, Inst *C,
                         InstContext &IC) {
  switch (K) {

  case Inst::Add:
  case Inst::Xor:
  case Inst::SAddSat:
  case Inst::UAddSat:
    // neither operand can be 0
    return IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C });

  case Inst::Sub:
  case Inst::SSubSat:
    // right operand cannot be 0
    return (OpNum == 0) ?
      IC.getConst(llvm::APInt(1, true)) :
      IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C });

  case Inst::USubSat:
    // left operand cannot be 0, right operand cannot be 0 or -1
    return (OpNum == 0) ?
      IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C }) : 
      IC.getInst(Inst::And, 1, {
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C }),
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt::getAllOnesValue(C->Width)), C })
      });

  case Inst::Mul:
    // neither operand can be 0 or 1
    return IC.getInst(Inst::And, 1, {
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C }),
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 1)), C })
      });

  case Inst::And:
  case Inst::Or:
    // neither operand can be 0 or -1
    return IC.getInst(Inst::And, 1, {
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C }),
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt::getAllOnesValue(C->Width)), C })
      });

  case Inst::Shl:
  case Inst::LShr:
    // left operand cannot be zero, right operand has to be > 0 and < Width
    return (OpNum == 0) ?
      IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C }) :
      IC.getInst(Inst::And, 1, {
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C }),
        IC.getInst(Inst::Ult, 1, { C, IC.getConst(llvm::APInt(C->Width, C->Width))})
      });

  case Inst::AShr:
    // left operand cannot be zero or -1, right operand has to be > 0 and < Width
    return (OpNum == 0) ?
      IC.getInst(Inst::And, 1, {
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C }),
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt::getAllOnesValue(C->Width)), C })
      }) :
      IC.getInst(Inst::And, 1, {
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C }),
        IC.getInst(Inst::Ult, 1, { C, IC.getConst(llvm::APInt(C->Width, C->Width))})
      });

  case Inst::FShl:
  case Inst::FShr:
    // handled elsewhere: first and second arguments can't both be zero
    // third argument has to be > 0 and < Width
    return (OpNum == 0 || OpNum == 1) ?
      IC.getConst(llvm::APInt(1, true)) :
      IC.getInst(Inst::And, 1, {
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C }),
        IC.getInst(Inst::Ult, 1, { C, IC.getConst(llvm::APInt(C->Width, C->Width)) })
      });

  case Inst::UDiv:
    // right operand can't be:
    // 0 (UB)
    // 1 (identity)
    // x /u 2  (prefer x >>u 1)
    // x /u -1 (prefer zext(x == -1))
    return (OpNum == 0) ?
      IC.getConst(llvm::APInt(1, true)) :
      IC.getInst(Inst::And, 1, {
        IC.getInst(Inst::Ult, 1, { IC.getConst(llvm::APInt(C->Width, 2)), C }),
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt::getAllOnesValue(C->Width)), C })
      });
    
  case Inst::SDiv:
  case Inst::SRem:
  case Inst::URem:
    // right operand can't be 0 or 1
    return (OpNum == 0) ?
      IC.getConst(llvm::APInt(1, true)) :
      IC.getInst(Inst::And, 1, {
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C }),
        IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 1)), C })
      });

  case Inst::Ult:
    // we don't want:
    //  0 <u  x (prefer x != 0)
    // -2 <u  x (prefer x == -1)
    // -1 <u  x (false)
    //  x <u -1 (prefer x != -1)
    //  x <u  0 (false)
    //  x <u  1 (prefer x == 1)
    return (OpNum == 0) ?
      IC.getInst(Inst::And, 1, {
          IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C }),
            IC.getInst(Inst::Ult, 1, { C, IC.getConst(llvm::APInt::getAllOnesValue(C->Width) - 1) })
      }) :
      IC.getInst(Inst::And, 1, {
          IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt::getAllOnesValue(C->Width)), C }),
          IC.getInst(Inst::Ult, 1, { IC.getConst(llvm::APInt(C->Width, 1)), C })
      });

  case Inst::Ule:
    // we don't want:
    //  0 <=u  x (true)
    //  1 <=u  x (prefer x != 0)
    // -1 <=u  x (prefer x == -1)
    //  x <=u -1 (true)
    //  x <=u -2 (prefer x != -1)
    //  x <=u  0 (prefer x == 0)
    return (OpNum == 0) ?
      IC.getInst(Inst::And, 1, {
          IC.getInst(Inst::Ult, 1, { IC.getConst(llvm::APInt(C->Width, 2)), C }),
          IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt::getAllOnesValue(C->Width)), C })
      }) :
      IC.getInst(Inst::And, 1, {
          IC.getInst(Inst::Ult, 1, { C, IC.getConst(llvm::APInt::getAllOnesValue(C->Width) - 1) }),
          IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt(C->Width, 0)), C })
      });    

  case Inst::Slt:
    // we don't want:
    // INT_MIN     <s x           (prefer x != INT_MIN)
    // INT_MAX - 1 <s x           (prefer x == INT_MAX)
    // INT_MAX     <s x           (false)
    // x           <s INT_MIN     (false)
    // x           <s INT_MIN + 1 (prefer x == INT_MIN)
    // x           <s INT_MAX     (prefer x != INT_MAX)
    return (OpNum == 0) ?
      IC.getInst(Inst::And, 1, {
          IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt::getSignedMinValue(C->Width)), C }),
          IC.getInst(Inst::Slt, 1, { C, IC.getConst(llvm::APInt::getSignedMaxValue(C->Width) - 1), C })
      }) :
      IC.getInst(Inst::And, 1, {
          IC.getInst(Inst::Slt, 1, { IC.getConst(llvm::APInt::getSignedMinValue(C->Width) + 1), C }),
          IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt::getSignedMaxValue(C->Width)), C })
      });

  case Inst::Sle: // FIXME
    // we don't want:
    // INT_MIN     <=s x           (true)
    // INT_MIN + 1 <=s x           (prefer x != INT_MIN)
    // INT_MAX     <=s x           (prefer x == INT_MAX)
    // x           <=s INT_MIN     (prefer x ==  INT_MIN)
    // x           <=s INT_MAX - 1 (prefer x != INT_MAX)
    // x           <=s INT_MAX     (true)
    return (OpNum == 0) ?
      IC.getInst(Inst::And, 1, {
          IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt::getSignedMaxValue(C->Width)), C }),
          IC.getInst(Inst::Slt, 1, { IC.getConst(llvm::APInt::getSignedMinValue(C->Width) + 1), C })
      }) :
      IC.getInst(Inst::And, 1, {
          IC.getInst(Inst::Slt, 1, { C, IC.getConst(llvm::APInt::getSignedMaxValue(C->Width) - 1) }),
          IC.getInst(Inst::Ne, 1, { IC.getConst(llvm::APInt::getSignedMinValue(C->Width)), C })
      });

  case Inst::Eq:
  case Inst::Ne:
  case Inst::SAddO:
  case Inst::UAddO:
  case Inst::SSubO:
  case Inst::USubO:
  case Inst::SMulO:
  case Inst::UMulO:
  case Inst::Select: // handled elsewhere: 2nd and 3rd arguments can't be same constant
    // no constraint
    return IC.getConst(llvm::APInt(1, true));

  default:
    llvm::report_fatal_error("unmatched: " + (std::string)Inst::getKindName(K));
  }
}

void addComplexConstraints(Inst *I,
                           Inst *&Constraints, const std::set<Inst *> &ConstSet,
                           InstContext &IC) {

  // TODO: consider pattern-matching and preventing:
  // --x
  // ~~x
  // 2 * x / 2
  
  // first and second arguments to funnel shift can't both be zero
  if (I->K == Inst::FShl || I->K == Inst::FShr) {
    if (ConstSet.find(I->Ops[0]) != ConstSet.end() &&
        ConstSet.find(I->Ops[1]) != ConstSet.end()) {
      auto Tmp0 = IC.getInst(Inst::Ne, 1,
          { IC.getConst(llvm::APInt(I->Ops[0]->Width, 0)), I->Ops[0] });
      auto Tmp1 = IC.getInst(Inst::Ne, 1,
          { IC.getConst(llvm::APInt(I->Ops[1]->Width, 0)), I->Ops[1] });
      auto Tmp2 = IC.getInst(Inst::Or, 1, { Tmp0, Tmp1 });
      Constraints = IC.getInst(Inst::And, 1, { Constraints, Tmp2 });
    }
  }

  // 2nd and 3rd arguments to select can't be the same constant
  if (I->K == Inst::Select) {
    if (ConstSet.find(I->Ops[1]) != ConstSet.end() &&
        ConstSet.find(I->Ops[2]) != ConstSet.end()) {
      auto Tmp = IC.getInst(Inst::Ne, 1, { I->Ops[1], I->Ops[2] });
      Constraints = IC.getInst(Inst::And, 1, { Constraints, Tmp });
    }
  }
}

static void visitConstants(Inst *I, std::set<Inst *> &Visited,
                           Inst *&Constraints, const std::set<Inst *> &ConstSet,
                           InstContext &IC, bool AvoidNops) {
  if (Visited.insert(I).second) {
    if (AvoidNops)
      addComplexConstraints(I, Constraints, ConstSet, IC);
    unsigned OpNum = 0;
    for (auto Op : I->Ops) {
      if (ConstSet.find(Op) != ConstSet.end()) {
        Constraints = IC.getInst(Inst::And, 1, { Constraints,
              AvoidNops ?
              getConstConstraint(I->K, OpNum, Op, IC) :
              getUBConstraint(I->K, OpNum, Op, IC)
        });
      } else {
        visitConstants(Op, Visited, Constraints, ConstSet, IC, AvoidNops);
      }
      ++OpNum;
    }
  }
}

std::error_code
ConstantSynthesis::synthesize(SMTLIBSolver *SMTSolver,
                              const BlockPCs &BPCs,
                              const std::vector<InstMapping> &PCs,
                              InstMapping Mapping, std::set<Inst *> &ConstSet,
                              std::map <Inst *, llvm::APInt> &ResultMap,
                              InstContext &IC, unsigned MaxTries, unsigned Timeout,
                              bool AvoidNops) {

  Inst *TrueConst = IC.getConst(llvm::APInt(1, true));
  Inst *FalseConst = IC.getConst(llvm::APInt(1, false));


  // generalization by substitution
  Inst *SubstAnte = TrueConst;
  Inst *TriedAnte = TrueConst;
  std::error_code EC;

  if (Pruner) {
    size_t Specializations = 0;
    for (auto &&VC : Pruner->getInputVals()) {
      if (Specializations++ >= MaxSpecializations) {
        break;
      }
      std::map<Inst *, llvm::APInt> VCCopy;
      for (auto Pair : VC) {
        if (Pair.second.hasValue()) {
          VCCopy[Pair.first] = Pair.second.getValue();
        }
      }
      if (!VCCopy.empty()) {
        std::map<Inst *, Inst *> InstCache;
        std::map<Block *, Block *> BlockCache;
        TriedAnte = IC.getInst(Inst::And, 1,
                    {IC.getInst(Inst::Eq, 1,
                    {getInstCopy(Mapping.LHS, IC, InstCache, BlockCache, &VCCopy, true),
                    getInstCopy(Mapping.RHS, IC, InstCache, BlockCache, &VCCopy, true)}), TriedAnte});
      }
    }
  }

  auto ConstConstraints = TrueConst;
  std::set<Inst *> Visited;
  visitConstants(Mapping.RHS, Visited, ConstConstraints, ConstSet, IC, AvoidNops);

  for (int I = 0; I < MaxTries; ++I)  {
    bool IsSat;
    std::vector<Inst *> ModelInstsFirstQuery;
    std::vector<llvm::APInt> ModelValsFirstQuery;

    // TriedAnte /\ SubstAnte
    Inst *FirstQueryAnte = IC.getInst(Inst::And, 1,
                                      { ConstConstraints,
                                        IC.getInst(Inst::And, 1, {SubstAnte, TriedAnte})});

    std::string Query = BuildQuery(IC, BPCs, PCs, InstMapping(Mapping.LHS, Mapping.RHS),
                                   &ModelInstsFirstQuery, FirstQueryAnte, true, true);

    if (Query.empty())
      return std::make_error_code(std::errc::value_too_large);

    EC = SMTSolver->isSatisfiable(Query, IsSat, ModelInstsFirstQuery.size(),
                                  &ModelValsFirstQuery, Timeout);

    if (EC) {
      if (DebugLevel > 3)
        llvm::errs() << "ConstantSynthesis: solver returns error on first query\n";
      return EC;
    }

    if (!IsSat) {
      // no constant found
      if (DebugLevel > 3) {
        llvm::errs() << "first query is UNSAT-- no more guesses\n";
      }
      return std::error_code();
    }

    if (DebugLevel > 3)
      llvm::errs() << "first query is SAT, returning the model:\n";

    Inst* TriedAnteLocal = FalseConst;
    std::map<Inst *, llvm::APInt> ConstMap;
    for (unsigned J = 0; J != ModelInstsFirstQuery.size(); ++J) {
      if (ConstSet.find(ModelInstsFirstQuery[J]) != ConstSet.end()) {
        if (DebugLevel > 3) {
          llvm::errs() << ModelInstsFirstQuery[J]->Name;
          llvm::errs() << ": ";
          llvm::errs() << ModelValsFirstQuery[J];
          llvm::errs() << "\n";
        }

        Inst *Const = IC.getConst(ModelValsFirstQuery[J]);
        ConstMap.insert(std::pair<Inst *, llvm::APInt>(ModelInstsFirstQuery[J], Const->Val));
        Inst *Ne = IC.getInst(Inst::Ne, 1, {ModelInstsFirstQuery[J], Const});
        if (ConstSet.size() == 1) {
          TriedAnteLocal = Ne;
        } else {
          TriedAnteLocal = IC.getInst(Inst::Or, 1, {TriedAnteLocal, Ne});
        }
      }
    }
    TriedAnte = IC.getInst(Inst::And, 1, {TriedAnte, TriedAnteLocal});

    std::map<Inst *, Inst *> InstCache;
    std::map<Block *, Block *> BlockCache;
    Inst *RHSCopy = getInstCopy(Mapping.RHS, IC, InstCache, BlockCache, &ConstMap, false);

    std::vector<Block *> Blocks = getBlocksFromPhis(Mapping.LHS);
    for (auto Block : Blocks) {
      Block->ConcretePred = 0;
    }

    if (DebugLevel > 2 && Pruner) {
      if (Pruner->isInfeasible(RHSCopy, DebugLevel)) {
        //TODO(manasij)
        llvm::errs() << "Second Query Skipping opportunity.\n";
      }
    }

    // check if the constant is valid for all inputs
    std::vector<Inst *> ModelInstsSecondQuery;
    std::vector<llvm::APInt> ModelValsSecondQuery;

    Query = BuildQuery(IC, BPCs, PCs, InstMapping(Mapping.LHS, RHSCopy),
                       &ModelInstsSecondQuery, 0);

    if (Query.empty())
      return std::make_error_code(std::errc::value_too_large);

    EC = SMTSolver->isSatisfiable(Query, IsSat, ModelInstsSecondQuery.size(),
                                  &ModelValsSecondQuery, Timeout);
    if (EC) {
      if (DebugLevel > 3) {
        llvm::errs()<<"ConstantSynthesis: solver returns error on second query\n";
      }
      return EC;
    }

    if (!IsSat) {
      if (DebugLevel > 3) {
        llvm::errs() << "second query is UNSAT-- this guess works\n";
      }
      ResultMap = std::move(ConstMap);
      return EC;
    } else {
      if (DebugLevel > 3) {
        llvm::errs() << "attempt " << I << ": second query is SAT-- constant doesn't work\n";
      }

      std::map<Inst *, llvm::APInt> SubstConstMap;
      ValueCache VC;
      for (unsigned J = 0; J != ModelInstsSecondQuery.size(); ++J) {
        Inst* Var = ModelInstsSecondQuery[J];
        if (Var->Name == BlockPred && !ModelValsSecondQuery[J].isNullValue())
          for (auto B : Blocks)
            for (unsigned I = 0 ; I < B->PredVars.size(); ++I)
              if (B->PredVars[I] == Var)
                B->ConcretePred = I + 1;

        if (ConstSet.find(Var) == ConstSet.end()) {
          SubstConstMap.insert(std::pair<Inst *, llvm::APInt>(Var, ModelValsSecondQuery[J]));
          VC.insert(std::pair<Inst *, llvm::APInt>(Var, ModelValsSecondQuery[J]));
        }
      }

      Inst *ConcreteLHS = nullptr;
      std::map<Inst *, Inst *> InstCache;
      std::map<Block *, Block *> BlockCache;
      if (EnableConcreteInterpreter) {
        ConcreteInterpreter CI(Mapping.LHS, VC);
        auto LHSV = CI.evaluateInst(Mapping.LHS);

        if (!LHSV.hasValue()) {
          llvm::report_fatal_error("the model returned from second query evaluates to poison for LHS");
        }
        ConcreteLHS = IC.getConst(LHSV.getValue());
      } else {
        ConcreteLHS = getInstCopy(Mapping.LHS, IC, InstCache,
                                  BlockCache, &SubstConstMap, true);
      }

      SubstAnte = IC.getInst(Inst::And, 1,
                             {IC.getInst(Inst::Eq, 1, {ConcreteLHS,
                                                       getInstCopy(Mapping.RHS, IC, InstCache,
                                                                   BlockCache, &SubstConstMap, true)}),
                              SubstAnte});
    }
  }

  if (DebugLevel > 3) {
    llvm::errs() << "number of constant synthesis tries exceeds MaxTries(";
    llvm::errs() << MaxTries;
    llvm::errs() << ")\n";
  }
  return EC;
}

std::error_code
ConstantSynthesisZ3::synthesize(SMTLIBSolver *SMTSolver,
                                const BlockPCs &BPCs,
                                const std::vector<InstMapping> &PCs,
                                InstMapping Mapping, std::set<Inst *> &ConstSet,
                                std::map <Inst *, llvm::APInt> &ResultMap,
                                InstContext &IC, unsigned MaxTries, unsigned Timeout,
                                bool AvoidNops) {
  // TODO: Replace with Z3 API implementation of constant synthesis
  // What to do about BlockPCs?

  Inst *TrueConst = IC.getConst(llvm::APInt(1, true));
  Inst *FalseConst = IC.getConst(llvm::APInt(1, false));

//  Inst *Ante = IC.getConst(llvm::APInt(1, true));
//  for (auto PC : PCs ) {
//    Inst *Eq = IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
//    Ante = IC.getInst(Inst::And, 1, {Ante, Eq});
//  }

  std::vector<Inst *> Vars;
  findVars(Mapping.LHS, Vars);

  Z3Driver Solver(Mapping.LHS, PCs, IC, BPCs);

  // generalization by substitution
  Inst *SubstAnte = TrueConst;
  Inst *TriedAnte = TrueConst;
  std::error_code EC;

  auto ConstConstraints = TrueConst;
  std::set<Inst *> Visited;
  visitConstants(Mapping.RHS, Visited, ConstConstraints, ConstSet, IC, AvoidNops);

  for (int I = 0; I < MaxTries; ++I)  {
    bool IsSat;
    std::vector<Inst *> ModelInstsFirstQuery;
    std::vector<llvm::APInt> ModelValsFirstQuery;

    // TriedAnte /\ SubstAnte
    Inst *FirstQueryAnte = IC.getInst(Inst::And, 1,
                                      { ConstConstraints,
                                        IC.getInst(Inst::And, 1, {SubstAnte, TriedAnte})});

    if (Solver.verify(Mapping.RHS, FirstQueryAnte, true)) {
      // lhs == rhs unsat
      // no constant found
      if (DebugLevel > 3) {
        llvm::errs() << "first query is UNSAT-- no more guesses\n";
      }
      return std::error_code();
    }

    // FIXME error handling
//    if (EC) {
//      if (DebugLevel > 3)
//        llvm::errs() << "ConstantSynthesis: solver returns error on first query\n";
//      return EC;
//    }


    if (DebugLevel > 3)
      llvm::errs() << "first query is SAT, returning the model:\n";

    Inst* TriedAnteLocal = FalseConst;
    std::map<Inst *, llvm::APInt> ConstMap;
    for (auto J : ConstSet) {
      auto Opt = Solver.getModelVal(J);
      if (!Opt.has_value()) {
        continue;
        // Is there a situation where we need a value but there isn't one?
      }
      llvm::APInt JVal = Opt.value();

      if (DebugLevel > 3) {
        llvm::errs() << J->Name;
        llvm::errs() << ": ";
        llvm::errs() << JVal;
        llvm::errs() << "\n";
      }
      Inst *Const = IC.getConst(JVal);
      ConstMap.insert(std::pair<Inst *, llvm::APInt>(J, Const->Val));
      Inst *Ne = IC.getInst(Inst::Ne, 1, {J, Const});
      if (ConstSet.size() == 1) {
        TriedAnteLocal = Ne;
      } else {
        TriedAnteLocal = IC.getInst(Inst::Or, 1, {TriedAnteLocal, Ne});
      }
    }

    TriedAnte = IC.getInst(Inst::And, 1, {TriedAnte, TriedAnteLocal});

    std::map<Inst *, Inst *> InstCache;
    std::map<Block *, Block *> BlockCache;
    Inst *RHSCopy = getInstCopy(Mapping.RHS, IC, InstCache, BlockCache, &ConstMap, false);

    std::vector<Block *> Blocks = getBlocksFromPhis(Mapping.LHS);
    for (auto Block : Blocks) {
      Block->ConcretePred = 0;
    }

    // TODO: Make Pruner use Z3Driver
//    if (DebugLevel > 2 && Pruner) {
//      if (Pruner->isInfeasible(RHSCopy, DebugLevel)) {
//        //TODO(manasij)
//        llvm::errs() << "Second Query Skipping opportunity.\n";
//      }
//    }

    if (Solver.verify(RHSCopy)) {
      // lhs != rhscopy unsat
      if (DebugLevel > 3) {
        llvm::errs() << "second query is UNSAT-- this guess works\n";
      }
      ResultMap = std::move(ConstMap);
      return EC;
    } else {
      // sat
      if (DebugLevel > 3) {
        llvm::errs() << "attempt " << I << ": second query is SAT-- constant doesn't work\n";
      }

      std::map<Inst *, llvm::APInt> SubstConstMap;
      ValueCache VC;
      for (auto Var : Vars) {
        auto Opt = Solver.getModelVal(Var);
        if (!Opt.has_value()) {
          continue;
          // Is there a situation where we need a value but there isn't one?
        }
        llvm::APInt VarVal = Opt.value();

        if (Var->Name == BlockPred && !VarVal.isNullValue())
          for (auto B : Blocks)
            for (unsigned I = 0 ; I < B->PredVars.size(); ++I)
              if (B->PredVars[I] == Var)
                B->ConcretePred = I + 1;

        if (ConstSet.find(Var) == ConstSet.end()) {
          SubstConstMap.insert(std::pair<Inst *, llvm::APInt>(Var, VarVal));
          VC.insert(std::pair<Inst *, llvm::APInt>(Var, VarVal));
        }
      }

      Inst *ConcreteLHS = nullptr;
      std::map<Inst *, Inst *> InstCache;
      std::map<Block *, Block *> BlockCache;
      if (EnableConcreteInterpreter) {
        ConcreteInterpreter CI(Mapping.LHS, VC);
        auto LHSV = CI.evaluateInst(Mapping.LHS);

        if (!LHSV.hasValue()) {
          llvm::report_fatal_error("the model returned from second query evaluates to poison for LHS");
        }
        ConcreteLHS = IC.getConst(LHSV.getValue());
      } else {
        ConcreteLHS = getInstCopy(Mapping.LHS, IC, InstCache,
                                  BlockCache, &SubstConstMap, true);
      }

      SubstAnte = IC.getInst(Inst::And, 1,
                             {IC.getInst(Inst::Eq, 1, {ConcreteLHS,
                                                       getInstCopy(Mapping.RHS, IC, InstCache,
                                                                   BlockCache, &SubstConstMap, true)}),
                              SubstAnte});
    }

  }

  if (DebugLevel > 3) {
    llvm::errs() << "number of constant synthesis tries exceeds MaxTries(";
    llvm::errs() << MaxTries;
    llvm::errs() << ")\n";
  }

  return EC;
}

}
