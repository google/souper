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

#include "souper/Infer/ExhaustiveSynthesis.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SetVector.h"
#include <queue>

using namespace souper;

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
  if (I->K == Inst::Var && (I->Name.compare("constant") == 0)) {
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

void specializeVars(Inst *Root, InstContext &IC, int Value) {
  std::set<Inst *> Visited;
  std::queue<Inst *> Q;
  Q.push(Root);
  while (!Q.empty()) {
    Inst *I = Q.front();
    Q.pop();
    if (!Visited.insert(I).second)
      continue;
    for (unsigned i = 0;  i < I->Ops.size() ; i++) {
      if (I->Ops[i]->K == Inst::Var && I->Ops[i]->Name.compare("constant"))
        I->Ops[i] = IC.getConst(APInt(I->Ops[i]->Width, Value));
      
      Q.push(I->Ops[i]);
    }
  }
}


void getGuesses (std::vector<Inst *>& Guesses,
                 std::vector<Inst* >& Vars,
                 std::vector<Inst *>& Inputs,
                 int Width, int LHSCost,
                 InstContext &IC) {
  
  int TooExpensive = 0;
  // start with the nops -- but not a constant since that is
  // legitimately faster to synthesize using the special-purpose
  // code above
  /*
    for (auto I : Inputs) {
    auto v = matchWidth(I, Width, IC);
    for (auto N : v)
    addGuess(N, LHSCost, Guesses, TooExpensive);
    }*/
  
  // TODO enforce permitted widths
  // TODO try both the source and dest width, if they're different
  /*
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
  */
  
  // binary and ternary instructions (TODO add div/rem)
  std::vector<Inst::Kind> Kinds = {
                                   Inst::Add, Inst::Sub, Inst::Mul,
                                   Inst::And, Inst::Or, Inst::Xor,
                                   Inst::Shl, Inst::AShr, Inst::LShr,
                                   Inst::Eq, Inst::Ne, Inst::Ult,
                                   Inst::Slt, Inst::Ule, Inst::Sle,
                                   Inst::Select,
  };

  Inputs.push_back(IC.getReserved());
  
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
            auto C = IC.createVar(OpWidth, "constant");
            v1.push_back(C);
          } else {
            v1 = matchWidth(*I, OpWidth, IC);
          }
          if ((*J)->K == Inst::Reserved) {
            auto C = IC.createVar(OpWidth, "constant");
            v2.push_back(C);
          } else {
            v2 = matchWidth(*J, OpWidth, IC);
          }
          
          
          for (auto v1i : v1) {
            for (auto v2i : v2) {
              if (K == Inst::Select) {
                for (auto L : Inputs) {
                  // PRUNE: a select's control input should never be constant
                  if (L->K == Inst::Reserved)
                    continue;
                  auto v3 = matchWidth(L, 1, IC);
                  auto N = IC.getInst(Inst::Select, OpWidth, { v3[0], v1i, v2i });
                  addGuess(N, LHSCost, Guesses, TooExpensive);
                }
              } else {
                // PRUNE: don't synthesize sub x, C since this is covered by add x, -C
                
                if (K == Inst::Sub && v2i->Name == "constant")
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
  }
}


std::error_code
ExhaustiveSynthesis::synthesize(SMTLIBSolver *SMTSolver,
                                const BlockPCs &BPCs,
                                const std::vector<InstMapping> &PCs,
                                Inst *LHS, Inst *&RHS,
                                InstContext &IC) {
  std::vector<Inst *> Vars;
  findVars(LHS, Vars);
  std::vector<Inst *> Inputs;
  // TODO tune the number of candidate inputs
  findCands(LHS, Inputs, /*WidthMustMatch=*/false, /*FilterVars=*/false, 15);
  
  llvm::outs() << "LHS has " << Vars.size() << " vars\n";
  
  std::vector<Inst *> Guesses;
  int LHSCost = souper::cost(LHS);
  getGuesses(Guesses, Vars, Inputs, LHS->Width ,LHSCost, IC);
  
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
  
  {
    Inst *Ante = IC.getConst(APInt(1, true));
    BlockPCs BPCsCopy;
    std::vector<InstMapping> PCsCopy;
    
    llvm::errs()<<Guesses.size();
    for (auto I : Guesses) {
      llvm::errs()<<"Guess\n";
      ReplacementContext RC;
      RC.printInst(I, llvm::errs(), true);
      
      // separate sub-expressions by copying vars
      std::map<Inst *, Inst *> InstCache;
      std::map<Block *, Block *> BlockCache;
      
      Inst *Ne = IC.getInst(Inst::Ne, 1, {getInstCopy(LHS, IC, InstCache, BlockCache, 0, true),
                                          getInstCopy(I, IC, InstCache, BlockCache, 0, true)});
      
      Ante = IC.getInst(Inst::And, 1, {Ante, Ne});
      separateBlockPCs(BPCs, BPCsCopy, InstCache, BlockCache, IC, 0, true);
      separatePCs(PCs, PCsCopy, InstCache, BlockCache, IC, 0, true);
    }
    
    llvm::errs()<<"BigQuery\n";
    ReplacementContext RC;
    RC.printInst(Ante, llvm::errs(), false);
    
    llvm::outs() << "there were " << Guesses.size() << " guesses but ";
    //      llvm::outs() << TooExpensive << " were too expensive\n";
    
    // (LHS != i_1) && (LHS != i_2) && ... && (LHS != i_n) == true
    InstMapping Mapping(Ante, IC.getConst(APInt(1, true)));
    std::string Query = BuildQuery(IC, BPCsCopy, PCsCopy, Mapping, 0, /*Negate=*/true);
    llvm::errs()<<Query<<"\n";
    if (Query.empty())
      return std::make_error_code(std::errc::value_too_large);
    bool BigQueryIsSat;
    EC = SMTSolver->isSatisfiable(Query, BigQueryIsSat, 0, 0, Timeout);
    BigQueryIsSat = true;
    if (EC)
      return EC;
    if (!BigQueryIsSat) {
      llvm::outs() << "big query is unsat, all done\n";
      return EC;
    } else {
      llvm::outs() << "big query is sat, looking for small queries\n";
    }
  }
  // find the valid one
  int unsat = 0;
  int GuessIndex = -1;
  for (auto I : Guesses) {
    GuessIndex++;
    {
      llvm::outs() << "\n\n--------------------------------------------\nguess " << GuessIndex << "\n";
      llvm::outs() << "\n";
    }
    
    std::vector<Inst *> ConstList;
    hasConstant(I, ConstList);
    
#if 0 // FIXME! need to create the mapping too
    if (ConstList.size() < 1) {
      std::string Query3 = BuildQuery(IC, BPCs, PCs, Mapping, 0);
      if (Query3.empty()) {
        llvm::outs() << "mt!\n";
        continue;
      }
      bool z;
      EC = SMTSolver->isSatisfiable(Query3, z, 0, 0, Timeout);
      if (EC) {
        llvm::outs() << "oops!\n";
        return EC;
      }
      if (!z) {
        llvm::outs() << "with no constants, this works\n";
        RHS = I;
        return EC;
      }
      continue;
    }
#endif
    
    // FIXME
    if (ConstList.size() > 1)
      llvm::report_fatal_error("yeah this test needs to get deleted");
    
    std::vector<llvm::APInt> BadConsts;
    int Tries = 0;
    
  again:
    if (Tries > 0)
      llvm::outs() << "\n\nagain:\n";
    
    // this SAT query will give us possible constants
    
    // FIXME fix values for vars
    
    // avoid choices for constants that have not worked out in previous iterations
    Inst *AvoidConsts = IC.getConst(APInt(1, true));
    for (auto C : BadConsts) {
      Inst *Ne = IC.getInst(Inst::Ne, 1, {IC.getConst(C), ConstList[0] });
      AvoidConsts = IC.getInst(Inst::And, 1, {AvoidConsts, Ne});
    }
    std::map<Inst *, llvm::APInt> ConstMap;
    
    {
      std::vector<Inst *> Vars;
      findVars(LHS, Vars);
      
      Inst *Ante = IC.getConst(APInt(1, true));
      for (int i = 1; i <= 3 ; i ++){
        std::map<Inst *, Inst *> InstCache;
        std::map<Block *, Block *> BlockCache;
        Inst *SpecializedLHS = getInstCopy(LHS, IC, InstCache, BlockCache, 0, true);
        Inst *SpecializedI = getInstCopy(I, IC, InstCache, BlockCache, 0, true);
        specializeVars(SpecializedLHS, IC, i);
        specializeVars(SpecializedI, IC, i);
        Ante = IC.getInst(Inst::And, 1, {Ante,
                                         IC.getInst(Inst::Eq, 1, {SpecializedLHS, SpecializedI})});
      }
      
      
      for (auto PC : PCs ) {
        ReplacementContext RC;
        RC.printInst(PC.LHS, llvm::errs(), true);
        RC.printInst(PC.RHS, llvm::errs(), true);
        std::vector<Inst *> ModelInsts;
        std::string PCQuery = BuildQuery(IC, BPCs, {}, PC, &ModelInsts, /*Negate=*/false);
        bool PCIsSat;
        std::vector<llvm::APInt> ModelVals;
        EC = SMTSolver->isSatisfiable(PCQuery, PCIsSat, ModelInsts.size(), &ModelVals, Timeout);
      }
      
      Ante = IC.getInst(Inst::And, 1, {Ante, IC.getInst(Inst::And, 1, {AvoidConsts, IC.getInst(Inst::Eq, 1, {LHS, I})})});
      ReplacementContext RC;
      RC.printInst(Ante, llvm::outs(), true);
      break;
      InstMapping Mapping(Ante,
                          IC.getConst(APInt(1, true)));
      
      std::vector<Inst *> ModelInsts;
      std::vector<llvm::APInt> ModelVals;
      std::string Query = BuildQuery(IC, BPCs, PCs, Mapping, &ModelInsts, /*Negate=*/true);
      
      if (Query.empty()) {
        llvm::outs() << "mt!\n";
        continue;
      }
      bool SmallQueryIsSat;
      EC = SMTSolver->isSatisfiable(Query, SmallQueryIsSat, ModelInsts.size(), &ModelVals, Timeout);
      if (EC) {
        llvm::outs() << "oops!\n";
        return EC;
      }
      if (!SmallQueryIsSat) {
        unsat++;
        llvm::outs() << "first query is unsat, all done with this guess\n";
        continue;
      }
      llvm::outs() << "first query is sat\n";
      
      for (unsigned J = 0; J != ModelInsts.size(); ++J) {
        if (ModelInsts[J]->Name == "constant") {
          auto Const = IC.getConst(ModelVals[J]);
          BadConsts.push_back(Const->Val);
          auto res = ConstMap.insert(std::pair<Inst *, llvm::APInt>(ModelInsts[J], Const->Val));
          llvm::outs() << "constant value = " << Const->Val << "\n";
          if (!res.second)
            llvm::report_fatal_error("constant already in map");
        }
      }
    }
    
    std::map<Inst *, Inst *> InstCache;
    std::map<Block *, Block *> BlockCache;
    BlockPCs BPCsCopy;
    std::vector<InstMapping> PCsCopy;
    auto I2 = getInstCopy(I, IC, InstCache, BlockCache, &ConstMap, false);
    separateBlockPCs(BPCs, BPCsCopy, InstCache, BlockCache, IC, &ConstMap, false);
    separatePCs(PCs, PCsCopy, InstCache, BlockCache, IC, &ConstMap, false);
    
    {
      llvm::outs() << "\n\nwith constant:\n";
      ReplacementContext RC;
      RC.printInst(LHS, llvm::outs(), true);
      llvm::outs() << "\n";
      RC.printInst(I2, llvm::outs(), true);
    }
    
    InstMapping Mapping2(LHS, I2);
    std::string Query2 = BuildQuery(IC, BPCsCopy, PCsCopy, Mapping2, 0);
    if (Query2.empty()) {
      llvm::outs() << "mt!\n";
      continue;
    }
    bool z;
    EC = SMTSolver->isSatisfiable(Query2, z, 0, 0, Timeout);
    if (EC) {
      llvm::outs() << "oops!\n";
      return EC;
    }
    if (z) {
        llvm::outs() << "second query is SAT-- constant doesn't work\n";
        Tries++;
        // TODO tune max tries
        if (Tries < 30)
          goto again;
    } else {
      llvm::outs() << "second query is UNSAT-- works for all values of this constant\n";
      RHS = I2;
      return EC;
    }
  }
  llvm::outs() << unsat << " were unsat.\n";
  // TODO maybe add back consistency checks between big and small queries
  
  return EC;
}


