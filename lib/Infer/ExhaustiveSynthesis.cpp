// Copyright 2018 The Souper Authors. All rights reserved.
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

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/CommandLine.h"
#include "souper/Infer/AliveDriver.h"
#include "souper/Infer/ConstantSynthesis.h"
#include "souper/Infer/ExhaustiveSynthesis.h"
#include "souper/Infer/Pruning.h"

#include <queue>
#include <functional>

static const unsigned MaxTries = 30;
static const unsigned MaxInputSpecializationTries = 2;
static const unsigned MaxLHSCands = 15;

bool UseAlive;
unsigned DebugLevel;

using namespace souper;
using namespace llvm;

static const std::vector<Inst::Kind> UnaryOperators = {
  Inst::CtPop, Inst::BSwap, Inst::BitReverse, Inst::Cttz, Inst::Ctlz
};

static const std::vector<Inst::Kind> BinaryOperators = {
  Inst::Add, Inst::Sub, Inst::Mul,
  Inst::And, Inst::Or, Inst::Xor,
  Inst::Shl, Inst::AShr, Inst::LShr,
  Inst::Eq, Inst::Ne, Inst::Ult,
  Inst::Slt, Inst::Ule, Inst::Sle,
  Inst::SAddSat, Inst::UAddSat,
  Inst::SSubSat, Inst::USubSat
};

namespace {
  static cl::opt<unsigned, /*ExternalStorage=*/true>
    DebugFlagParser("souper-exhaustive-synthesis-debug-level",
    cl::desc("Synthesis debug level (default=0). "
    "The larger the number is, the more fine-grained debug "
    "information will be printed"),
    cl::Hidden, cl::location(DebugLevel), cl::init(0));
  static cl::opt<unsigned> MaxNumInstructions("souper-exhaustive-synthesis-num-instructions",
    cl::desc("Maximum number of instructions to synthesize (default=1)."),
    cl::init(1));
  static cl::opt<bool> EnableBigQuery("souper-exhaustive-synthesis-enable-big-query",
    cl::desc("Enable big query in exhaustive synthesis (default=false)"),
    cl::init(false));
  static cl::opt<bool, /*ExternalStorage=*/true>
    AliveFlagParser("souper-use-alive", cl::desc("Use Alive2 as the backend"),
    cl::Hidden, cl::location(UseAlive), cl::init(false));
  static cl::opt<bool> LSBPruning("souper-lsb-pruning",
    cl::desc("Try to prune guesses by looking for a difference in LSB"),
    cl::init(false));
  static cl::opt<bool> EnableDataflowPruning("souper-dataflow-pruning",
    cl::desc("Enable pruning based on dataflow analysis (default=false)"),
    cl::init(false));
  static cl::opt<bool> SynthesisConstWithCegisLoop("souper-synthesis-const-with-cegis",
    cl::desc("Synthesis constants with CEGIS (default=false)"),
    cl::init(false));
}

// TODO
// tune the constants at the top of the file
// see and obey the ignore-cost command line flag
// constant synthesis
//   try to first make small constants? -128-255?
// multiple instructions
//   make a fresh constant per new binary/ternary instruction
// call solver in batches -- need to work on batch size
//   tune batch size -- can be a lot bigger for simple RHSs
//   or look for feedback from solver, timeouts etc.
// make sure path conditions work as expected
// synthesize x.with.overflow
// remove nop synthesis
// once an optimization works we can try adding UB qualifiers on the RHS
//   probably almost as good as synthesizing these directly
// prune a subtree once it becomes clear it isn't a cost win
// aggressively prune dumb instructions
//   which logical operations are equivalent at 1 bit? (include icmps in this)
// add constraints guarding against synthesizing dumb constants
//   add x, 0; sub x, 0; shift x, 0; mul x, 0; mul x, 1, mul x, 2
//   shift left by 1
// aggressively prune dumb instruction sequences
//   trunc of trunc, sext of sext, zext of zext
//   trunc to i1 vs. %w = and %v, 1; %x = icmp ne %w, 0
//   a bloom filter or something
//   use the solver to recognize non-minimal instructions / instruction sequences?
// we want to avoid guessing code that's already there on the LHS
//   hashing?
// test against CEGIS with LHS components
// test the width matching stuff
// take outside uses into account -- only in the cost model?
// experiment with synthesizing at reduced bitwidth, then expanding the result
// aggressively avoid calling into the solver

void addGuess(Inst *RHS, int MaxCost, std::vector<Inst *> &Guesses,
              int &TooExpensive) {
  if (souper::cost(RHS) < MaxCost)
    Guesses.push_back(RHS);
  else
    TooExpensive++;
}

typedef std::function<bool(Inst *, std::vector<Inst *> &)> PruneFunc;

// Does a short-circuiting AND operation
PruneFunc MkPruneFunc(std::vector<PruneFunc> Funcs) {
  return [Funcs](Inst *I, std::vector<Inst *> &RI) {
    for (auto F : Funcs) {
      if (!F(I, RI)) {
        return false;
      }
    }
    return true;
  };
}

// return false if successfully proved infeasible
bool CostPrune(Inst *I, std::vector<Inst *> &ReservedInsts) {

  // Cost exceeds LHS
  if (!ReservedInsts.empty() && instCount(I) >= MaxNumInstructions)
    return false;

//   if (ReservedInsts.empty() && instCount(I) > MaxNumInstructions)
//     return false;
// TODO : Handle this logic here instead of comparing against TooExpensive
//  at arbitrary places in the synthesis algorithm

  return true;
}

void getGuesses(std::vector<Inst *> &Guesses,
                const std::vector<Inst *> &Inputs,
                int Width, int LHSCost,
                InstContext &IC, Inst *PrevInst, Inst *PrevSlot,
                int &TooExpensive,
                PruneFunc prune) {

  std::vector<Inst *> PartialGuesses;

  std::vector<Inst *> Comps(Inputs.begin(), Inputs.end());

  // Conversion Operators
  for (auto Comp : Comps) {
    if (Comp->Width == Width)
      continue;

    if (Width > Comp->Width) {
      auto NSExt = IC.getInst(Inst::SExt, Width, { Comp });
      auto NZExt = IC.getInst(Inst::ZExt, Width, { Comp });
      addGuess(NSExt, LHSCost, PartialGuesses, TooExpensive);
      addGuess(NZExt, LHSCost, PartialGuesses, TooExpensive);
    } else {
      auto NTrunc = IC.getInst(Inst::Trunc, Width, { Comp });
      addGuess(NTrunc, LHSCost, PartialGuesses, TooExpensive);
    }
  }

  Inst *I1 = IC.getReservedInst();
  Comps.push_back(I1);

  // Unary Operators
  if (Width > 1) {
    for (auto K : UnaryOperators) {
      for (auto Comp : Comps) {
        if (Comp->K == Inst::ReservedInst) {
          auto V = IC.createHole(Width);
          auto N = IC.getInst(K, Width, { V });
          addGuess(N, LHSCost, PartialGuesses, TooExpensive);
          continue;
        }

        if (Comp->Width != Width)
          continue;

        // Prune: unary operation on constant
        if (Comp->K == Inst::ReservedConst)
          continue;

        if (K == Inst::BSwap && Width % 16 != 0)
          continue;

        auto N = IC.getInst(K, Width, { Comp });
        addGuess(N, LHSCost, PartialGuesses, TooExpensive);
      }
    }
  }

  // Binary instructions (TODO add div/rem)
  Inst *C1 = IC.getReservedConst();
  Comps.push_back(C1);
  // reservedinst starts with width 0
  Inst *I2 = IC.getReservedInst();
  Comps.push_back(I2);

  for (auto K : BinaryOperators) {
    if (Inst::isCmp(K) && Width != 1)
      continue;

    // PRUNE: one-bit shifts don't make sense
    if (Inst::isShift(K) && Width == 1)
      continue;

    for (auto I = Comps.begin(); I != Comps.end(); ++I) {
      // Prune: only one of (mul x, C), (mul C, x) is allowed
      if (Inst::isCommutative(K) && (*I)->K == Inst::ReservedConst)
        continue;

      // Prune: I1 should only be the first argument
      if ((*I)->K == Inst::ReservedInst && (*I) != I1)
        continue;

      // PRUNE: don't try commutative operators both ways
      auto Start = Inst::isCommutative(K) ? I : Comps.begin();
      for (auto J = Start; J != Comps.end(); ++J) {
        // Prune: I2 should only be the second argument
        if ((*J)->K == Inst::ReservedInst && (*J) != I2)
          continue;

        // PRUNE: never useful to div, rem, sub, and, or, xor,
        // icmp, select a value against itself
        if ((*I == *J) && (Inst::isCmp(K) || K == Inst::And || K == Inst::Or ||
                           K == Inst::Xor || K == Inst::Sub || K == Inst::UDiv ||
                           K == Inst::SDiv || K == Inst::SRem || K == Inst::URem ||
                           K == Inst::Select))
          continue;

        // PRUNE: never operate on two constants
        if ((*I)->K == Inst::ReservedConst && (*J)->K == Inst::ReservedConst)
          continue;

        // see if we need to make a var representing a constant
        // that we don't know yet

        Inst *V1, *V2;
        if (Inst::isCmp(K)) {

          if ((*I)->Width == 0 && (*J)->Width == 0) {
            // TODO: support (cmp hole, hole);
            continue;
          }

          if ((*I)->Width == 0) {
            if ((*I)->K == Inst::ReservedConst) {
              // (cmp const, comp)
              V1 = IC.createSynthesisConstant((*J)->Width, (*I)->SynthesisConstID);
            } else if ((*I)->K == Inst::ReservedInst) {
              // (cmp hole, comp)
              V1 = IC.createHole((*J)->Width);
            }
          } else {
            V1 = *I;
          }

          if ((*J)->Width == 0) {
            if ((*J)->K == Inst::ReservedConst) {
              // (cmp comp, const)
              V2 = IC.createSynthesisConstant((*I)->Width, (*J)->SynthesisConstID);
            } else if ((*J)->K == Inst::ReservedInst) {
              // (cmp comp, hole)
              V2 = IC.createHole((*I)->Width);
            }
          } else {
            V2 = *J;
          }
        } else {
          if ((*I)->K == Inst::ReservedConst) {
            // (binop const, comp)
            V1 = IC.createSynthesisConstant(Width, (*I)->SynthesisConstID);
          } else if ((*I)->K == Inst::ReservedInst) {
            // (binop hole, comp)
            V1 = IC.createHole(Width);
          } else {
            V1 = *I;
          }

          if ((*J)->K == Inst::ReservedConst) {
            // (binop comp, const)
            V2 = IC.createSynthesisConstant(Width, (*J)->SynthesisConstID);
          } else if ((*J)->K == Inst::ReservedInst) {
            // (binop comp, hole)
            V2 = IC.createHole(Width);
          } else {
            V2 = *J;
          }
        }

        if (V1->Width != V2->Width)
          continue;

        if (!Inst::isCmp(K) && V1->Width != Width)
          continue;

        // PRUNE: don't synthesize sub x, C since this is covered by add x, -C
        if (K == Inst::Sub && V2->SynthesisConstID != 0)
          continue;

        auto N = IC.getInst(K, Inst::isCmp(K) ? 1 : Width, { V1, V2 });
        addGuess(N, LHSCost, PartialGuesses, TooExpensive);
      }
    }
  }

  // Deal with select instruction separately, since some guesses might
  // need two reserved per select instruction
  Inst *C2 = IC.getReservedConst();
  Comps.push_back(C2);
  Inst *I3 = IC.getReservedInst();
  Comps.push_back(I3);

  for (auto I = Comps.begin(); I != Comps.end(); ++I) {
    if ((*I)->K == Inst::ReservedInst && (*I) != I1)
      continue;
    if ((*I)->K == Inst::ReservedConst && (*I) != C1)
      continue;

    for (auto J = Comps.begin(); J != Comps.end(); ++J) {
      if ((*J)->K == Inst::ReservedInst && (*J) != I2)
        continue;
      if ((*J)->K == Inst::ReservedConst && (*J) != C2)
        continue;

      // Prune (select cond, x, x)
      if (I == J)
        continue;
      Inst *V1, *V2;
      if ((*I)->K == Inst::ReservedConst) {
        V1 = IC.createSynthesisConstant(Width, (*I)->SynthesisConstID);
      } else if ((*I)->K == Inst::ReservedInst) {
        V1 = IC.createHole(Width);
      } else {
        V1 = *I;
      }
      if ((*J)->K == Inst::ReservedConst) {
        V2 = IC.createSynthesisConstant(Width, (*J)->SynthesisConstID);
      } else if ((*J)->K == Inst::ReservedInst) {
        V2 = IC.createHole(Width);
      } else {
        V2 = *J;
      }

      if (V1->Width != V2->Width)
        continue;

      if (V1->Width != Width)
        continue;

      assert(V1->Width == V2->Width);

      for (auto L : Comps) {
        if (L->K == Inst::ReservedInst && L != I3)
          continue;

        // (select i1, c, c)
        // PRUNE: a select's control input should never be constant
        if (L->K == Inst::ReservedConst)
          continue;

        Inst *V;
        if (L->K == Inst::ReservedInst) {
          V = IC.createHole(1);
        } else {
          V = L;
        }

        if (V->Width != 1)
          continue;


        auto SelectInst = IC.getInst(Inst::Select, Width, { V, V1, V2 });

        addGuess(SelectInst, LHSCost, PartialGuesses, TooExpensive);
      }
    }
  }

  for (auto I : PartialGuesses) {
    Inst *JoinedGuess;
    // if it is the first time the function getGuesses() gets called, then
    // leave it as the root and do not plug it to any other insts
    if (!PrevInst)
      JoinedGuess = I;
    else {
      // plugin the new guess I to PrevInst
      JoinedGuess = instJoin(PrevInst, PrevSlot, I, IC);
    }

    // get all empty slots from the newly plugged inst
    std::vector<Inst *> CurrSlots;
    getHoles(JoinedGuess, CurrSlots);

    // if no empty slot, then push the guess to the result list
    if (CurrSlots.empty()) {
      std::vector<Inst *> empty;
      if (prune(JoinedGuess, empty)) {
        Guesses.push_back(JoinedGuess);
      }
      continue;
    }

    // if there exist empty slots, then call getGuesses() recursively
    // and fill the empty slots
    if (prune(JoinedGuess, CurrSlots)) {
      for (auto S : CurrSlots)
        getGuesses(Guesses, Inputs, S->Width,
                   LHSCost, IC, JoinedGuess, S, TooExpensive, prune);
    }
  }
}

APInt getNextInputVal(Inst *Var,
                      Inst *LHSUB,
                      const BlockPCs &BPCs,
                      const std::vector<InstMapping> &PCs,
                      std::map<Inst *, std::vector<llvm::APInt>> &TriedVars,
                      InstContext &IC,
                      SMTLIBSolver *SMTSolver,
                      unsigned Timeout,
                      bool &HasNextInputValue) {

  // TODO
  // Specialize input values respecting blockpcs
  if (!BPCs.empty()) {
    HasNextInputValue = false;
    return APInt(Var->Width, 0);
  }

  HasNextInputValue = true;
  Inst *Ante = IC.getConst(APInt(1, true));
  Ante = IC.getInst(Inst::And, 1, {Ante, LHSUB});
  for (auto PC : PCs ) {
    Inst* Eq = IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
    Inst* PCUB = getUBInstCondition(IC, Eq);
    Ante = IC.getInst(Inst::And, 1, {Ante, Eq});
    Ante = IC.getInst(Inst::And, 1, {Ante, PCUB});
  }

  // If a variable is neither found in PCs or TriedVar, return APInt(0)
  bool VarHasTried = TriedVars.find(Var) != TriedVars.end();
  if (!VarHasTried) {
    std::vector<Inst *> VarsInPCs;
    souper::findVars(Ante, VarsInPCs);
    if (std::find(VarsInPCs.begin(), VarsInPCs.end(), Var) == VarsInPCs.end()) {
      TriedVars[Var].push_back(APInt(Var->Width, 0));
      return APInt(Var->Width, 0);
    }
  }

  for (auto Value : TriedVars[Var]) {
    Inst* Ne = IC.getInst(Inst::Ne, 1, {Var, IC.getConst(Value)});
    Ante = IC.getInst(Inst::And, 1, {Ante, Ne});
  }

  InstMapping Mapping(Ante, IC.getConst(APInt(1, true)));

  std::vector<Inst *> ModelInsts;
  std::vector<llvm::APInt> ModelVals;
  std::string Query = BuildQuery(IC, {}, {}, Mapping, &ModelInsts, 0, /*Negate=*/ true);

  bool PCQueryIsSat;
  std::error_code EC;
  EC = SMTSolver->isSatisfiable(Query, PCQueryIsSat, ModelInsts.size(), &ModelVals, Timeout);

  if (EC || !PCQueryIsSat) {
    if (VarHasTried) {
      // If we previously generated guesses on Var, and the query becomes
      // unsat, then clear the state and call the getNextInputVal() again to
      // get a new guess
      TriedVars.erase(Var);
      return getNextInputVal(Var, LHSUB, BPCs, PCs, TriedVars, IC,
                             SMTSolver, Timeout, HasNextInputValue);
    } else {
      // No guess record for Var found and query tells unsat, we can conclude
      // that no possible guess there
      HasNextInputValue = false;
      return APInt(Var->Width, 0);
    }
  }
  if (DebugLevel > 3)
    llvm::errs() << "Input guess SAT\n";
  for (unsigned I = 0 ; I != ModelInsts.size(); I++) {
    if (ModelInsts[I] == Var) {
      TriedVars[Var].push_back(ModelVals[I]);
      if (DebugLevel > 3)
        llvm::errs() << "Guess input value = " << ModelVals[I] << "\n";
      return ModelVals[I];
    }
  }

  llvm::report_fatal_error("Model does not contain the guess input variable");
}

Inst *findConst(souper::Inst *I,
                std::set<const Inst *> &Visited) {
  if (I->K == Inst::Var && I->SynthesisConstID != 0) {
    return I;
  } else {
    for (auto &&Op : I->Ops) {
      if (Visited.find(Op) == Visited.end()) {
        auto Ret = findConst(Op, Visited);
        if (Ret) {
          return Ret;
        }
      }
    }
    Visited.insert(I);
  }
  return nullptr;
}

bool exceeds64Bits(const Inst *I, std::set<const Inst *> &Visited) {
  if (I->Width > 64) {
    return true;
  } else {
    for (auto &&Op : I->Ops) {
      if (Visited.find(Op) == Visited.end()) {
        if (exceeds64Bits(Op, Visited)) {
          return true;
        }
      }
    }
    Visited.insert(I);
  }
  return false;
}

bool canDifferInLSB(SynthesisContext &SC, Inst *RHSGuess) {
  Inst *LHSOne = SC.IC.getConst(llvm::APInt(SC.LHS->Width, 1));
  Inst *NewLHS = SC.IC.getInst(Inst::And, SC.LHS->Width, {SC.LHS, LHSOne});
  Inst *RHSOne = SC.IC.getConst(llvm::APInt(RHSGuess->Width, 1));
  Inst *NewRHS = SC.IC.getInst(Inst::And, RHSGuess->Width, {RHSGuess, RHSOne});
  // TODO: Experiment with larger masks: 3, 7, MSB, etc.

  InstMapping NewMapping{NewLHS, NewRHS};

  auto Query = BuildQuery(SC.IC, SC.BPCs, SC.PCs, NewMapping, 0, 0);

  bool QueryIsSat;
  auto EC = SC.SMTSolver->isSatisfiable(Query, QueryIsSat, 0, 0, SC.Timeout);
  if (EC) {
    if (DebugLevel > 1)
      llvm::errs() << "Solver error in LSB pruning!\n";
    return false;
  }
  return QueryIsSat;
}

std::error_code synthesizeWithAlive(SynthesisContext &SC, Inst *&RHS,
                                    const std::vector<souper::Inst *> &Guesses) {
  std::error_code EC;
  std::map<Inst *, Inst *> InstCache;
  std::map<Block *, Block *> BlockCache;
  std::set<const Inst *> Visited;
  if (exceeds64Bits(SC.LHS, Visited))
    llvm::report_fatal_error("LHS exceeds 64 bits");

  Inst *Ante = SC.IC.getConst(APInt(1, true));
  for (auto PC : SC.PCs ) {
    Inst *Eq = SC.IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
    Ante = SC.IC.getInst(Inst::And, 1, {Ante, Eq});
  }

  AliveDriver Verifier(SC.LHS, Ante, SC.IC);
  for (auto &&G : Guesses) {
    std::set<const Inst *> Visited;
    auto C = findConst(G, Visited);
    if (!C) {
      if (Verifier.verify(G)) {
        RHS = G;
        return EC;
      }
    } else {
      if (SynthesisConstWithCegisLoop) {
        auto ConstMap = Verifier.synthesizeConstantsWithCegis(G, SC.IC);
        if (ConstMap.empty())
          continue;
        auto GWithC = getInstCopy(G, SC.IC, InstCache, BlockCache, &ConstMap,
                                  /*CloneVars=*/false);
        RHS = GWithC;
        return EC;
      } else {
        auto ConstMap = Verifier.synthesizeConstants(G);
        // TODO: Counterexample guided loop or UB constraints in query

        auto GWithC = getInstCopy(G, SC.IC, InstCache, BlockCache, &ConstMap,
                                  /*CloneVars=*/false);
        if (Verifier.verify(GWithC)) {
          RHS = GWithC;
          return EC;
        } else {
          continue;
        }
        return EC;
      }
    }
  }
  return EC;
}

bool isBigQuerySat(SynthesisContext &SC,
                   const std::vector<souper::Inst *> &Guesses) {
  // Big Query
  // TODO: Need to check if big query actually saves us time or just wastes time
  std::error_code EC;
  std::map<Inst *, Inst *> InstCache;
  std::map<Block *, Block *> BlockCache;
  Inst *Ante = SC.IC.getConst(APInt(1, true));
  BlockPCs BPCsCopy;
  std::vector<InstMapping> PCsCopy;

  for (auto I : Guesses) {
    // separate sub-expressions by copying vars
    std::map<Inst *, Inst *> InstCache;
    std::map<Block *, Block *> BlockCache;

    Inst *Eq = SC.IC.getInst(Inst::Eq, 1,
                         {getInstCopy(SC.LHS, SC.IC, InstCache, BlockCache, 0, true),
                          getInstCopy(I, SC.IC, InstCache, BlockCache, 0, true)});

    Ante = SC.IC.getInst(Inst::And, 1, {Ante, Eq});
    separateBlockPCs(SC.BPCs, BPCsCopy, InstCache, BlockCache, SC.IC, 0, true);
    separatePCs(SC.PCs, PCsCopy, InstCache, BlockCache, SC.IC, 0, true);
  }

  if (DebugLevel > 2) {
    llvm::errs() << "\n\n--------------------------------------------\nBigQuery\n";
    ReplacementContext RC;
    RC.printInst(Ante, llvm::errs(), false);
  }

  // (LHS != i_1) && (LHS != i_2) && ... && (LHS != i_n) == true
  InstMapping Mapping(Ante, SC.IC.getConst(APInt(1, true)));
  std::string Query = BuildQuery(SC.IC, BPCsCopy, PCsCopy, Mapping, 0, 0, /*Negate=*/false);
  if (Query.empty()) {
    if (DebugLevel > 2)
      llvm::errs() << "Big Query is too big, skipping\n";
    return false;
  }
  bool BigQueryIsSat;
  EC = SC.SMTSolver->isSatisfiable(Query, BigQueryIsSat, 0, 0, SC.Timeout);
  if (EC)
    return false;
  if (!BigQueryIsSat) {
    if (DebugLevel > 2)
      llvm::errs() << "big query is unsat, all done\n";
  } else {
    if (DebugLevel > 2)
      llvm::errs() << "big query is sat, looking for small queries\n";
  }
  return BigQueryIsSat;
}

void generateAndSortGuesses(SynthesisContext &SC,
                            std::vector<Inst *> &Guesses) {
  std::vector<Inst *> Cands;
  findCands(SC.LHS, Cands, /*WidthMustMatch=*/false, /*FilterVars=*/false, MaxLHSCands);
  if (DebugLevel > 1)
    llvm::errs() << "got " << Cands.size() << " candidates from LHS\n";

  int LHSCost = souper::cost(SC.LHS, /*IgnoreDepsWithExternalUses=*/true);

  int TooExpensive = 0;

  std::vector<Inst *> Inputs;
  findVars(SC.LHS, Inputs);
  PruningManager DataflowPruning(SC, Inputs, DebugLevel);
  // Cheaper tests go first
  std::vector<PruneFunc> PruneFuncs = {CostPrune};
  if (EnableDataflowPruning) {
    DataflowPruning.init();
    PruneFuncs.push_back(DataflowPruning.getPruneFunc());
  }
  auto PruneCallback = MkPruneFunc(PruneFuncs);
  // TODO(zhengyangl): Refactor the syntactic pruning into a
  // prune function here, between Cost and Dataflow
  // TODO(manasij7479) : If RHS is concrete, evaluate both sides
  // TODO(regehr?) : Solver assisted pruning (should be the last component)

  getGuesses(Guesses, Cands, SC.LHS->Width,
             LHSCost, SC.IC, nullptr, nullptr, TooExpensive, PruneCallback);
  if (DebugLevel >= 1) {
    DataflowPruning.printStats(llvm::errs());
  }

  // add nops guesses separately
  for (auto I : Inputs) {
    if (I->Width == SC.LHS->Width)
      addGuess(I, LHSCost, Guesses, TooExpensive);
  }

  // one of the real advantages of this approach to synthesis vs
  // CEGIS is that we can synthesize in precisely increasing cost
  // order, and not try to somehow teach the solver how to do that
  std::stable_sort(Guesses.begin(), Guesses.end(),
                   [](Inst *a, Inst *b) -> bool {
                     return souper::cost(a) < souper::cost(b);
                   });

  if (DebugLevel > 1)
    llvm::errs() << "There are " << Guesses.size() << " Guesses\n";
}



typedef std::map<Inst*, std::vector<llvm::APInt>> InstConstList;

#if (false)
std::map<Inst *, llvm::APInt>
findSatisfyingConstantMap(SynthesisContext &SC, InstConstList &BadConsts,
                          InstConstList &TriedVars,
                          std::vector<Inst *> ConstList,
                          std::vector<Inst *> Vars,
                          Inst *RHSGuess,int &UnsatCounter) {
  // this SAT query will give us possible constants

  // avoid choices for constants that have not worked out in previous iterations
  // ((R1 != C11 ) \/ (R2 != C21 )) /\ ((R1 != C12 ) \/ (R2 != C22 )) /\ ...
  std::error_code EC;
  std::map<Inst *, Inst *> InstCache;
  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, llvm::APInt> ConstMap;

  Inst *AvoidConsts = SC.IC.getConst(APInt(1, true));
  if (!BadConsts.empty()) {
    for (unsigned i = 0; i < BadConsts[ConstList[0]].size(); ++i) {
      Inst *Ante = SC.IC.getConst(APInt(1, false));
      for (auto C : ConstList) {
        Inst *Ne = SC.IC.getInst(Inst::Ne, 1, {SC.IC.getConst(BadConsts[C][i]), C });
        Ante = SC.IC.getInst(Inst::Or, 1, {Ante, Ne});
      }
      AvoidConsts = SC.IC.getInst(Inst::And, 1, {Ante, AvoidConsts});
    }
  }

  Inst *Ante = SC.IC.getConst(APInt(1, true));
  if (!Vars.empty()) {

    // Currently MaxInputSpecializationTries can be set to any
    // non-negative number. However, a limitation here is: since the current
    // implementation of getNextInputVal() does not specialize a input
    // variable with two same value, if some program has input type
    // (i1, i32), because the type of the first argument is i1, there will
    // be only two specialized input combinations, such as (false, C_1) and
    // (true, C_2).
    // TODO: getNextInputVal() need to be more flexible
    for (unsigned It = 0; It < MaxInputSpecializationTries; It++) {
      bool HasNextInputValue = false;

      std::map<Inst *, llvm::APInt> VarMap;
      for (auto Var: Vars) {
        APInt NextInput = getNextInputVal(Var, SC.LHSUB, SC.BPCs, SC.PCs, TriedVars, SC.IC,
                                          SC.SMTSolver, SC.Timeout,
                                          HasNextInputValue);
        if (!HasNextInputValue)
          break;
        VarMap.insert(std::pair<Inst *, llvm::APInt>(Var, NextInput));
      }
      if (!HasNextInputValue)
        break;

      std::map<Inst *, Inst *> InstCache;
      std::map<Block *, Block *> BlockCache;
      Inst *Eq =
        SC.IC.getInst(Inst::Eq, 1,
                    {getInstCopy(SC.LHS, SC.IC, InstCache, BlockCache, &VarMap, true),
                    getInstCopy(RHSGuess, SC.IC, InstCache, BlockCache, &VarMap, true)});
      Ante = SC.IC.getInst(Inst::And, 1, {Eq, Ante});
    }
  }
  Ante = SC.IC.getInst(Inst::And, 1, {SC.IC.getInst(Inst::Eq, 1, {SC.LHS, RHSGuess}), Ante});
  Ante = SC.IC.getInst(Inst::And, 1, {AvoidConsts, Ante});
  if (DebugLevel > 3) {
    ReplacementContext RC;
    RC.printInst(RHSGuess, llvm::errs(), true);
  }
  InstMapping Mapping(Ante,
                      SC.IC.getConst(APInt(1, true)));

  std::vector<Inst *> ModelInsts;
  std::vector<llvm::APInt> ModelVals;
  std::string Query = BuildQuery(SC.IC, SC.BPCs, SC.PCs, Mapping, &ModelInsts, 0, /*Negate=*/true);

  bool FirstSmallQueryIsSat;
  EC = SC.SMTSolver->isSatisfiable(Query, FirstSmallQueryIsSat,
                                ModelInsts.size(), &ModelVals, SC.Timeout);
  if (EC) {
    if (DebugLevel > 1)
      llvm::errs() << "error!\n";
    return {};
  }
  if (!FirstSmallQueryIsSat) {
    UnsatCounter++;
    if (DebugLevel > 2)
      llvm::errs() << "first query is unsat, all done with this guess\n";
    return {};
  }
  if (DebugLevel > 3)
    llvm::errs() << "first query is sat\n";

  for (unsigned J = 0; J != ModelInsts.size(); ++J) {
    if (ModelInsts[J]->Name.find(ReservedConstPrefix) != std::string::npos) {
      auto Const = SC.IC.getConst(ModelVals[J]);
      BadConsts[ModelInsts[J]].push_back(Const->Val);
      auto res = ConstMap.insert(std::pair<Inst *, llvm::APInt>(ModelInsts[J], Const->Val));
      if (DebugLevel > 3)
        llvm::errs() << "constant value = " << Const->Val << "\n";
      if (!res.second)
        llvm::report_fatal_error("constant already in map");
    }
  }
  return ConstMap;
}
#endif

std::error_code isConcreteCandidateSat(
  SynthesisContext &SC, std::map<Inst *, llvm::APInt> &ConstMap,
  Inst *RHSGuess, bool &IsSat) {

  std::error_code EC;
  BlockPCs BPCsCopy;
  std::vector<InstMapping> PCsCopy;
  std::map<Inst *, Inst *> InstCache;
  std::map<Block *, Block *> BlockCache;
  separateBlockPCs(SC.BPCs, BPCsCopy, InstCache, BlockCache, SC.IC, &ConstMap, false);
  separatePCs(SC.PCs, PCsCopy, InstCache, BlockCache, SC.IC, &ConstMap, false);

  InstMapping Mapping(SC.LHS, RHSGuess);

  std::string Query2 = BuildQuery(SC.IC, BPCsCopy, PCsCopy, Mapping, 0, 0);

  EC = SC.SMTSolver->isSatisfiable(Query2, IsSat, 0, 0, SC.Timeout);
  if (EC && DebugLevel > 1) {
    llvm::errs() << "verification query failed!\n";
  }
  return EC;
}

std::error_code
ExhaustiveSynthesis::synthesize(SMTLIBSolver *SMTSolver,
                                const BlockPCs &BPCs,
                                const std::vector<InstMapping> &PCs,
                                Inst *LHS, Inst *&RHS,
                                InstContext &IC, unsigned Timeout) {
  SynthesisContext SC{IC, SMTSolver, LHS, getUBInstCondition(SC.IC, SC.LHS), PCs, BPCs, Timeout};

  std::vector<Inst *> Guesses;
  std::error_code EC;
  generateAndSortGuesses(SC, Guesses);

  if (Guesses.empty()) {
    return EC;
  }

  std::map<Inst *, Inst *> InstCache;
  std::map<Block *, Block *> BlockCache;

  if (UseAlive) {
    return synthesizeWithAlive(SC, RHS, Guesses);
  } else {
    if (EnableBigQuery && isBigQuerySat(SC,Guesses)) {
      return EC; // None of the guesses work
    }
  }
  // find the valid one
  int GuessIndex = -1;

  std::vector<Inst *> Vars;
  souper::findVars(LHS, Vars);

  for (auto I : Guesses) {
    GuessIndex++;
    if (DebugLevel > 2) {
      llvm::errs() << "\n--------------------------------------------\nguess " << GuessIndex << "\n\n";
      ReplacementContext RC;
      RC.printInst(I, llvm::errs(), /*printNames=*/true);
      llvm::errs() << "\n";
    }

    std::set<Inst *> ConstSet;
    souper::getConstants(I, ConstSet);
    bool GuessHasConstant = !ConstSet.empty();
    std::map <Inst *, llvm::APInt> ResultConstMap;
    if (!GuessHasConstant) {
      bool IsSAT;

      EC = isConcreteCandidateSat(SC, ResultConstMap, I, IsSAT);
      if (EC) {
        return EC;
      }
      if (IsSAT) {
        if (DebugLevel > 3)
          llvm::errs() << "second query is SAT-- constant doesn't work\n";
        continue;
      } else {
        if (DebugLevel > 3)
          llvm::errs() << "query is UNSAT\n";
        RHS = I;
        return EC;
      }
    } else {
      // guess has constant

      ConstantSynthesis CS;

      EC = CS.synthesize(SMTSolver, BPCs, PCs, InstMapping (LHS, I), ConstSet,
                         ResultConstMap, IC, /*MaxTries=*/MaxTries, Timeout);
      if (!ResultConstMap.empty()) {
        RHS = getInstCopy(I, IC, InstCache, BlockCache, &ResultConstMap, false);
        return EC;
      } else {
        continue;
      }
    }
  }

  return EC;
}
