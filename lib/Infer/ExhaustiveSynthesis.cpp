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
#include "souper/Infer/ExhaustiveSynthesis.h"

#include <queue>

using namespace souper;
using namespace llvm;

namespace {
  static cl::opt<unsigned> DebugLevel("souper-exhaustive-synthesis-debug-level",
    cl::desc("Synthesis debug level (default=0). "
    "The larger the number is, the more fine-grained debug "
    "information will be printed"),
    cl::init(0));
}


// TODO
// see and obey the ignore-cost command line flag
// nove this code to its own file
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

// TODO move this to Inst
const bool isCmp(Inst::Kind K) {
  return K == Inst::Eq || K == Inst::Ne || K == Inst::Ult ||
    K == Inst::Slt || K == Inst::Ule || K == Inst::Sle;
}

// TODO move this to Inst
const bool isShift(Inst::Kind K) {
  return K == Inst::Shl || K == Inst::AShr || K == Inst::LShr;
}

void hasConstantHelper(Inst *I, std::set<Inst *> &Visited,
                       std::vector<Inst *> &ConstList) {
  // FIXME this only works for one constant and keying by name is bad
  if (I->K == Inst::Var && (I->Name.find("reserved_") != std::string::npos)) {
    // FIXME use a less stupid sentinel
    ConstList.push_back(I);
  } else {
    if (Visited.insert(I).second)
      for (auto Op : I->Ops)
        hasConstantHelper(Op, Visited, ConstList);
  }
}

// TODO do this a more efficient way
// TODO do this a better way, checking for constants by name is dumb
void hasConstant(Inst *I, std::vector<Inst *> &ConstList) {
  std::set<Inst *> Visited;
  hasConstantHelper(I, Visited, ConstList);
}

// TODO small vector by reference?
std::vector<Inst *> matchWidth(Inst *I, unsigned NewW, InstContext &IC) {
  int OldW = isCmp(I->K) ? 1 : I->Width;
  if (OldW > NewW)
    return { IC.getInst(Inst::Trunc, NewW, { I }) };
  if (OldW < NewW)
    return {
            IC.getInst(Inst::SExt, NewW, { I }),
            IC.getInst(Inst::ZExt, NewW, { I }),
    };
  return { I };
}

void addGuess(Inst *RHS, int MaxCost, std::vector<Inst *> &Guesses,
              int &TooExpensive) {
  if (souper::cost(RHS) < MaxCost)
    Guesses.push_back(RHS);
  else
    TooExpensive++;
}

void findVars(Inst *Root, std::vector<Inst *> &Vars) {
  // breadth-first search
  std::set<Inst *> Visited;
  std::queue<Inst *> Q;
  Q.push(Root);
  while (!Q.empty()) {
    Inst *I = Q.front();
    Q.pop();
    if (!Visited.insert(I).second)
      continue;
    if (I->K == Inst::Var)
      Vars.push_back(I);
    for (auto Op : I->Ops)
      Q.push(Op);
  }
}


void getGuesses(std::vector<Inst *>& Guesses,
                std::vector<Inst* >& Vars,
                std::vector<Inst *>& Inputs,
                int Width, int LHSCost,
                InstContext &IC) {

  int TooExpensive = 0;
  // start with the nops -- but not a constant since that is
  // legitimately faster to synthesize using the special-purpose code
  for (auto I : Inputs) {
    auto v = matchWidth(I, Width, IC);
    for (auto N : v)
      addGuess(N, LHSCost, Guesses, TooExpensive);
  }

  // TODO enforce permitted widths
  // TODO try both the source and dest width, if they're different
  std::vector<Inst::Kind> Unary = {
                                   Inst::CtPop, Inst::BSwap, Inst::Cttz, Inst::Ctlz
  };
  if (Width > 1) {
    for (auto K : Unary) {
      for (auto I : Inputs) {
        auto v1 = matchWidth(I, Width, IC);
        for (auto v1i : v1) {
          auto N = IC.getInst(K, Width, { v1i });
          addGuess(N, LHSCost, Guesses, TooExpensive);
        }
      }
    }
  }

  // binary and ternary instructions (TODO add div/rem)
  std::vector<Inst::Kind> Kinds = {Inst::Add, Inst::Sub, Inst::Mul,
                                   Inst::And, Inst::Or, Inst::Xor,
                                   Inst::Shl, Inst::AShr, Inst::LShr,
                                   Inst::Eq, Inst::Ne, Inst::Ult,
                                   Inst::Slt, Inst::Ule, Inst::Sle,
  };

  Inputs.push_back(IC.getReserved());

  // Binary and Unary operators
  for (auto K : Kinds) {
    for (auto I = Inputs.begin(); I != Inputs.end(); ++I) {
      // PRUNE: don't try commutative operators both ways
      auto Start = Inst::isCommutative(K) ? I : Inputs.begin();
      for (auto J = Start; J != Inputs.end(); ++J) {
        // PRUNE: never useful to div, rem, sub, and, or, xor,
        // icmp, select a value against itself
        if ((*I == *J) && (isCmp(K) || K == Inst::And || K == Inst::Or ||
                           K == Inst::Xor || K == Inst::Sub || K == Inst::UDiv ||
                           K == Inst::SDiv || K == Inst::SRem || K == Inst::URem ||
                           K == Inst::Select))
          continue;
        // PRUNE: never operate on two constants
        if ((*I)->K == Inst::Reserved && (*J)->K == Inst::Reserved)
          continue;
        /*
         * there are three obvious choices of width for an
         * operator: left input, right input, or output, so try
         * them all (in the future we'd also like to explore
         * things like synthesizing double-width operations)
         */
        llvm::SmallSetVector<int, 4> Widths;
        if ((*I)->K != Inst::Reserved)
          Widths.insert((*I)->Width);
        if ((*J)->K != Inst::Reserved)
          Widths.insert((*J)->Width);
        if (!isCmp(K))
          Widths.insert(Width);
        if (Widths.size() < 1)
          llvm::report_fatal_error("no widths to work with");
        
        for (auto OpWidth : Widths) {
          if (OpWidth < 1)
            llvm::report_fatal_error("bad width");
          // PRUNE: one-bit shifts don't make sense
          if (isShift(K) && OpWidth == 1)
            continue;
          
          // see if we need to make a var representing a constant
          // that we don't know yet
          std::vector<Inst *> v1, v2;
          if ((*I)->K == Inst::Reserved) {
            auto C = IC.createVar(OpWidth, (*I)->Name);
            v1.push_back(C);
          } else {
            v1 = matchWidth(*I, OpWidth, IC);
          }
          if ((*J)->K == Inst::Reserved) {
            auto C = IC.createVar(OpWidth, (*J)->Name);
            v2.push_back(C);
          } else {
            v2 = matchWidth(*J, OpWidth, IC);
          }
          
          
          for (auto v1i : v1) {
            for (auto v2i : v2) {
              // PRUNE: don't synthesize sub x, C since this is covered by add x, -C
              if (K == Inst::Sub && v2i->Name.find("reserved_") != std::string::npos)
                continue;
              auto N = IC.getInst(K, isCmp(K) ? 1 : OpWidth, { v1i, v2i });
              auto v4 = matchWidth(N, Width, IC);
              for (auto v4i : v4) {
                addGuess(v4i, LHSCost, Guesses, TooExpensive);
              }
            }
          }
        }
      }
    }
  }

  // Deal with select instruction separately, since some guesses might
  // need two reserved per select instruction
  Inputs.push_back(IC.getReserved());
  for (auto I = Inputs.begin(); I != Inputs.end(); ++I) {
    for (auto J = Inputs.begin(); J != Inputs.end(); ++J) {
      if (I == J) continue;
      std::vector<Inst *> v1, v2;
      if ((*I)->K == Inst::Reserved) {
        auto C = IC.createVar(Width, (*I)->Name);
        v1.push_back(C);
      } else {
        v1 = matchWidth(*I, Width, IC);
      }
      if ((*J)->K == Inst::Reserved) {
        auto C = IC.createVar(Width, (*J)->Name);
        v2.push_back(C);
      } else {
        v2 = matchWidth(*J, Width, IC);
      }

      for (auto L : Inputs) {
        for (auto v1i : v1) {
          for (auto v2i : v2) {
            // PRUNE: a select's control input should never be constant
            if (L->K == Inst::Reserved)
              continue;
            auto v3 = matchWidth(L, 1, IC);
            auto N = IC.getInst(Inst::Select, Width, { v3[0], v1i, v2i });
            addGuess(N, LHSCost, Guesses, TooExpensive);
          }
        }
      }
    }
  }
}
APInt getNextInputVal(Inst *Var,
                      const std::vector<InstMapping> &PCs,
                      std::map<Inst*, std::vector<llvm::APInt>> &TriedVars,
                      InstContext &IC,
                      SMTLIBSolver *SMTSolver,
                      unsigned Timeout,
                      bool &HasNextInputValue) {

  HasNextInputValue = true;
  Inst *Ante = IC.getConst(APInt(1, true));
  for (auto PC : PCs ) {
    Inst* Eq = IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
    Ante = IC.getInst(Inst::And, 1, {Ante, Eq});
  }

  // If a variable is neither found in PCs or TriedVar, return APInt(0)
  if (TriedVars.find(Var) == TriedVars.end()) {
    std::vector<Inst *> VarsInPCs;
    findVars(Ante, VarsInPCs);
    if (std::find(VarsInPCs.begin(), VarsInPCs.end(), Var) == VarsInPCs.end()) {
      TriedVars[Var].push_back(APInt(Var->Width, 0));
      return APInt(Var->Width, 0);
    }
  }
  
  for (auto Value: TriedVars[Var]) {
    Inst* Ne = IC.getInst(Inst::Ne, 1, {Var, IC.getConst(Value)});
    Ante = IC.getInst(Inst::And, 1, {Ante, Ne});
  }

  InstMapping Mapping(Ante, IC.getConst(APInt(1, true)));

  std::vector<Inst *> ModelInsts;
  std::vector<llvm::APInt> ModelVals;
  std::string Query = BuildQuery(IC, {}, {}, Mapping, &ModelInsts, /*Negate=*/ true);

  bool PCQueryIsSat;
  std::error_code EC;
  EC = SMTSolver->isSatisfiable(Query, PCQueryIsSat, ModelInsts.size(), &ModelVals, Timeout);

  if (EC || !PCQueryIsSat) {
    HasNextInputValue = false;
    return APInt(Var->Width, 0);
  }
  if (DebugLevel > 2)
    llvm::errs()<<"Variable Guess SAT";
  for (unsigned I = 0 ; I != ModelInsts.size(); I ++) {
    if (ModelInsts[I] == Var) {
      TriedVars[Var].push_back(ModelVals[I]);
      if (DebugLevel > 2)
        llvm::errs()<<"var value = " << ModelVals[I] <<"\n";
      return ModelVals[I];
    }
  }

  llvm::report_fatal_error("shouldn't be here");
}

std::error_code
ExhaustiveSynthesis::synthesize(SMTLIBSolver *SMTSolver,
                                const BlockPCs &BPCs,
                                const std::vector<InstMapping> &PCs,
                                Inst *LHS, Inst *&RHS,
                                InstContext &IC, unsigned Timeout) {
  std::vector<Inst *> Vars;
  findVars(LHS, Vars);
  std::vector<Inst *> Inputs;
  // TODO tune the number of candidate inputs
  findCands(LHS, Inputs, /*WidthMustMatch=*/false, /*FilterVars=*/false, 15);
  
  llvm::outs() << "LHS has " << Vars.size() << " vars\n";
  
  std::vector<Inst *> Guesses;
  int LHSCost = souper::cost(LHS);
  getGuesses(Guesses, Vars, Inputs, LHS->Width, LHSCost, IC);
  
  std::error_code EC;
  if (Guesses.size() < 1)
    return EC;
  
  // one of the real advantages of this approach to synthesis vs
  // CEGIS is that we can synthesize in precisely increasing cost
  // order, and not try to somehow teach the solver how to do that
  std::sort(Guesses.begin(), Guesses.end(),
            [](Inst *a, Inst *b) -> bool {
              return souper::cost(a) < souper::cost(b);
            });

  // Big Query
  {
    Inst *Ante = IC.getConst(APInt(1, true));
    BlockPCs BPCsCopy;
    std::vector<InstMapping> PCsCopy;

    if (DebugLevel > 2)    
      llvm::errs()<<Guesses.size();

    for (auto I : Guesses) {
      if (DebugLevel > 2) {
        llvm::errs()<<"Guess\n";
        ReplacementContext RC;
        RC.printInst(I, llvm::errs(), true);
      }

      // separate sub-expressions by copying vars
      std::map<Inst *, Inst *> InstCache;
      std::map<Block *, Block *> BlockCache;
      
      Inst *Ne = IC.getInst(Inst::Ne, 1, {getInstCopy(LHS, IC, InstCache, BlockCache, 0, true),
                                          getInstCopy(I, IC, InstCache, BlockCache, 0, true)});

      Ante = IC.getInst(Inst::And, 1, {Ante, Ne});
      separateBlockPCs(BPCs, BPCsCopy, InstCache, BlockCache, IC, 0, true);
      separatePCs(PCs, PCsCopy, InstCache, BlockCache, IC, 0, true);
    }

    if (DebugLevel > 2) {
      llvm::errs()<<"BigQuery\n";
      ReplacementContext RC;
      RC.printInst(Ante, llvm::errs(), false);
    }

    // (LHS != i_1) && (LHS != i_2) && ... && (LHS != i_n) == true
    InstMapping Mapping(Ante, IC.getConst(APInt(1, true)));
    std::string Query = BuildQuery(IC, BPCsCopy, PCsCopy, Mapping, 0, /*Negate=*/true);
    if (Query.empty())
      return std::make_error_code(std::errc::value_too_large);
    bool BigQueryIsSat;
    EC = SMTSolver->isSatisfiable(Query, BigQueryIsSat, 0, 0, Timeout);
    BigQueryIsSat = true;
    if (EC)
      return EC;
    if (!BigQueryIsSat) {
      if (DebugLevel > 2)
        llvm::outs() << "big query is unsat, all done\n";
      return EC;
    } else {
      if (DebugLevel > 2)
        llvm::outs() << "big query is sat, looking for small queries\n";
    }
  }
  // find the valid one
  int unsat = 0;
  int GuessIndex = -1;
  for (auto I : Guesses) {
    GuessIndex++;
    if (DebugLevel > 2) {
      llvm::outs() << "\n\n--------------------------------------------\nguess " << GuessIndex << "\n";
      llvm::outs() << "\n";
    }

    std::vector<Inst *> ConstList;
    hasConstant(I, ConstList);

    std::map<Inst*, std::vector<llvm::APInt>> BadConsts;
    int Tries = 0;

  again:
    if (Tries > 0 && DebugLevel > 2)
      llvm::outs() << "\n\nagain:\n";

    // this SAT query will give us possible constants

    // avoid choices for constants that have not worked out in previous iterations
    // ((R1 != C11 ) \/ (R2 != C21 )) /\ ((R1 != C12 ) \/ (R2 != C22 )) /\ ...
    Inst *AvoidConsts = IC.getConst(APInt(1, true));
    if (!BadConsts.empty()) {
      for (unsigned i = 0; i < BadConsts[ConstList[0]].size(); ++i) {
        Inst *Ante = IC.getConst(APInt(1, false));
        for (auto C : ConstList) {
          Inst *Ne = IC.getInst(Inst::Ne, 1, {IC.getConst(BadConsts[C][i]), C });
          Ante = IC.getInst(Inst::Or, 1, {Ante, Ne});
        }
        AvoidConsts = IC.getInst(Inst::And, 1, {Ante, AvoidConsts});
      }
    }

    std::vector<Inst *> Vars;
    findVars(LHS, Vars);

    std::map<Inst *, std::vector<llvm::APInt>> TriedVars;
    std::map<Inst *, llvm::APInt> ConstMap;

    {
      Inst *Ante = IC.getConst(APInt(1, true));
      if (!Vars.empty()) {
        // Try three combinations of values at a time
        for (unsigned It = 0; It < 2; It ++) {
          bool HasNextInputValue = false;

          std::map<Inst *, llvm::APInt> VarMap;
          for (auto Var: Vars) {
            APInt NextInput = getNextInputVal(Var, PCs, TriedVars, IC,
                                              SMTSolver, Timeout,
                                              HasNextInputValue);
            if (!HasNextInputValue) break;
            VarMap.insert(std::pair<Inst *, llvm::APInt>(Var, NextInput));
          }
          if (!HasNextInputValue) break;

          std::map<Inst *, Inst *> InstCache;
          std::map<Block *, Block *> BlockCache;
          Inst *Eq =
            IC.getInst(Inst::Eq, 1,
                       {getInstCopy(LHS, IC, InstCache, BlockCache, &VarMap, true),
                        getInstCopy(I, IC, InstCache, BlockCache, &VarMap, true)});
          Ante = IC.getInst(Inst::And, 1, {Eq, Ante});
        }
      }
      Ante = IC.getInst(Inst::And, 1, {IC.getInst(Inst::Eq, 1, {LHS, I}), Ante});
      Ante = IC.getInst(Inst::And, 1, {AvoidConsts, Ante});
      InstMapping Mapping(Ante,
                          IC.getConst(APInt(1, true)));

      std::vector<Inst *> ModelInsts;
      std::vector<llvm::APInt> ModelVals;
      std::string Query = BuildQuery(IC, BPCs, PCs, Mapping, &ModelInsts, /*Negate=*/true);

      bool FirstSmallQueryIsSat;
      EC = SMTSolver->isSatisfiable(Query, FirstSmallQueryIsSat,
                                    ModelInsts.size(), &ModelVals, Timeout);
      if (EC) {
        return EC;
      }
      if (!FirstSmallQueryIsSat) {
        unsat++;
        if (DebugLevel > 2)
          llvm::outs() << "first query is unsat, all done with this guess\n";
        continue;
      }
      if (DebugLevel > 2)
        llvm::outs() << "first query is sat\n";

      for (unsigned J = 0; J != ModelInsts.size(); ++J) {
        if (ModelInsts[J]->Name.find("reserved_") != std::string::npos) {
          auto Const = IC.getConst(ModelVals[J]);
          BadConsts[ModelInsts[J]].push_back(Const->Val);
          auto res = ConstMap.insert(std::pair<Inst *, llvm::APInt>(ModelInsts[J], Const->Val));
          if (DebugLevel > 2)
            llvm::outs() << "constant value = " << Const->Val << "\n";
          if (!res.second)
            llvm::report_fatal_error("constant already in map");
        }
      }
    }

    BlockPCs BPCsCopy;
    std::vector<InstMapping> PCsCopy;
    std::map<Inst *, Inst *> InstCache;
    std::map<Block *, Block *> BlockCache;
    auto I2 = getInstCopy(I, IC, InstCache, BlockCache, &ConstMap, false);
    separateBlockPCs(BPCs, BPCsCopy, InstCache, BlockCache, IC, &ConstMap, false);
    separatePCs(PCs, PCsCopy, InstCache, BlockCache, IC, &ConstMap, false);

    InstMapping Mapping2(LHS, I2);
    std::string Query2 = BuildQuery(IC, BPCsCopy, PCsCopy, Mapping2, 0);

    bool SecondSmallQueryIsSat;
    EC = SMTSolver->isSatisfiable(Query2, SecondSmallQueryIsSat, 0, 0, Timeout);
    if (EC) {
      return EC;
    }
    if (SecondSmallQueryIsSat) {
      if (DebugLevel > 2)
        llvm::outs() << "second query is SAT-- constant doesn't work\n";
      Tries++;
      // TODO tune max tries
      if (Tries < 30)
        goto again;
    } else {
      if (DebugLevel > 2)
        llvm::outs() << "second query is UNSAT-- works for all values of this constant\n";
      RHS = I2;
      return EC;
    }
  }
  if (DebugLevel > 2)
    llvm::outs() << unsat << " were unsat.\n";
  // TODO maybe add back consistency checks between big and small queries
  
  return EC;
}


