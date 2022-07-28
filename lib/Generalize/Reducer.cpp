#include "llvm/Support/KnownBits.h"
#include "souper/Generalize/Reducer.h"
#include "souper/Infer/SynthUtils.h"

#define _LIBCPP_DISABLE_DEPRECATION_WARNINGS

namespace souper {

void collectInsts(Inst *I, std::set<Inst *> &Results) {
  std::vector<Inst *> Stack{I};
  while (!Stack.empty()) {
    auto Current = Stack.back();
    Stack.pop_back();

    Results.insert(Current);

    for (auto Child : Current->Ops) {
      if (Results.find(Child) == Results.end()) {
        Stack.push_back(Child);
      }
    }
  }
}

ParsedReplacement Reducer::ReduceGreedy(ParsedReplacement Input) {
  std::set<Inst *> Insts;
  collectInsts(Input.Mapping.LHS, Insts);
  // TODO: topological sort, to reduce number of solver calls
  // Try to remove one instruction at a time
  int failcount = 0;
  std::set<Inst *> Visited;
  do {
    auto It = Insts.begin();
    auto I = *It;
    Insts.erase(It);
    if (Visited.find(I) != Visited.end()) {
      continue;
    }
    Visited.insert(I);
    if (!safeToRemove(I, Input)) {
      continue;
    }
    auto Copy = Input;
    Eliminate(Input, I);

    if (!VerifyInput(Input)) {
      Input = Copy;
      failcount++;
      if (failcount >= Insts.size()) {
        break;
      }
      continue;
    }
    Insts.clear();
    collectInsts(Input.Mapping.LHS, Insts);
  } while (!Insts.empty());
  return Input;
}

// Eventually replace the functions in Preconditions{.h/.cpp} with this.
// Does not produce exhaustive result. TODO Have an option to wrap in a cegis loop.
bool Reducer::inferKBPrecondition(ParsedReplacement &Input, std::vector<Inst *> Targets) {
  assert(Targets.size() == 1 && "Multiple targets unimplemented");
  std::map<Inst *, llvm::KnownBits> Result;
  std::set<Inst *> SymConsts;
  std::map<Inst *, Inst *> InstCache;
  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, llvm::APInt> ConstMap;
  size_t ConstID = 0;
  for (auto V : Targets) {
    auto C = IC.createSynthesisConstant(V->Width, ConstID++);
    InstCache[V] = C;
    SymConsts.insert(C);
  }
  auto Copy = Input;
  InstMapping Rep;
  Rep.LHS = getInstCopy(Input.Mapping.LHS, IC, InstCache, BlockCache, &ConstMap, false, false);
  Rep.RHS = getInstCopy(Input.Mapping.RHS, IC, InstCache, BlockCache, &ConstMap, false, false);

  ConstantSynthesis CS;

//      llvm::errs() << "Constant synthesis problem: \n";
//      Input.print(llvm::errs(), true);
//      llvm::errs() << "....end.... \n";

  if (auto EC = CS.synthesize(S->getSMTLIBSolver(), Input.BPCs, Input.PCs,
                              Rep, SymConsts, ConstMap, IC, 30, 60, false)) {
    llvm::errs() << "Constant Synthesis internal error : " <<  EC.message();
  }

  if (ConstMap.empty()) {
    if (DebugLevel > 3) {
      llvm::errs() << "Constant Synthesis failed, moving on.\n";
    }
    Input = Copy;
  } else {
    InstCache.clear();

    // TODO: Generalize before allowing multiple targets
    auto NewVar = Targets[0];
    auto C = InstCache[NewVar];

    InstCache[C] = NewVar;

    NewVar->KnownOnes = ConstMap[C];
    NewVar->KnownZeros = ~ConstMap[C];

    // Give up if can't be weakened 'too much'
    const size_t WeakeningThreshold = NewVar->Width/2;
    size_t BitsWeakened = 0;

    for (size_t i = 0; i < NewVar->Width; ++i) {
      auto SaveZero = NewVar->KnownZeros;
      auto SaveOne = NewVar->KnownOnes;

      NewVar->KnownZeros.clearBit(i);
      NewVar->KnownOnes.clearBit(i);

      if (!VerifyInput(Input)) {
        NewVar->KnownZeros = SaveZero;
        NewVar->KnownOnes = SaveOne;
      } else {
        BitsWeakened++;
      }
    }
    if (BitsWeakened < WeakeningThreshold) {
      Input = Copy; // Reset to old state
      return false;
    } else {
      return true;
    }
  }
  return false;
}

ParsedReplacement Reducer::ReduceGreedyKBIFY(ParsedReplacement Input) {
  std::set<Inst *> Insts;
  collectInsts(Input.Mapping.LHS, Insts);
  // TODO: topological sort, to reduce number of solver calls
  // Try to remove one instruction at a time
  size_t failcount = 0;
  std::set<Inst *> Visited;
  do {
    auto It = Insts.begin();
    auto I = *It;
    Insts.erase(It);
    if (Visited.find(I) != Visited.end()) {
      continue;
    }
    Visited.insert(I);
    if (!safeToRemove(I, Input) || I->Width == 1 /*1 bit KB doesn't make sense*/) {
      continue;
    }
    auto Copy = Input;
    auto NewVar = Eliminate(Input, I);

    // Input won't verify because ReduceGreedy has been called before this.

    // Try to replace NewVar with a symbolic constant and do constant synthesis.
    Inst *C = IC.createSynthesisConstant(NewVar->Width, 0);
    std::map<Inst *, Inst *> InstCache = {{NewVar, C}};
    std::map<Block *, Block *> BlockCache;
    std::map<Inst *, llvm::APInt> ConstMap;

    InstMapping Rep;
    Rep.LHS = getInstCopy(Input.Mapping.LHS, IC, InstCache, BlockCache, &ConstMap, false, false);
    Rep.RHS = getInstCopy(Input.Mapping.RHS, IC, InstCache, BlockCache, &ConstMap, false, false);

    ConstantSynthesis CS;
    std::set<Inst *> ConstSet{C};

//      llvm::errs() << "Constant synthesis problem: \n";
//      Input.print(llvm::errs(), true);
//      llvm::errs() << "....end.... \n";

    if (auto EC = CS.synthesize(S->getSMTLIBSolver(), Input.BPCs, Input.PCs,
                                Rep, ConstSet, ConstMap, IC, 30, 60, false)) {
      llvm::errs() << "Constant Synthesis internal error : " <<  EC.message();
    }

    if (ConstMap.empty()) {
      if (DebugLevel > 3) {
        llvm::errs() << "Constant Synthesis failed, moving on.\n";
      }
      Input = Copy;
      failcount++;
      if (failcount >= Insts.size()) {
        break;
      }
    } else {
      InstCache.clear();
      InstCache[C] = NewVar;

      NewVar->KnownOnes = ConstMap[C];
      NewVar->KnownZeros = ~ConstMap[C];

      // Give up if can't be weakened 'too much'
      const size_t WeakeningThreshold = NewVar->Width/2;
      size_t BitsWeakened = 0;

      for (size_t i = 0; i < NewVar->Width; ++i) {
        auto SaveZero = NewVar->KnownZeros;
        auto SaveOne = NewVar->KnownOnes;

        NewVar->KnownZeros.clearBit(i);
        NewVar->KnownOnes.clearBit(i);

        if (!VerifyInput(Input)) {
          NewVar->KnownZeros = SaveZero;
          NewVar->KnownOnes = SaveOne;
        } else {
          BitsWeakened++;
        }
      }
      if (BitsWeakened < WeakeningThreshold) {
        Input = Copy;
        failcount++;
        if (failcount >= Insts.size()) {
          break;
        }
      }
    }
    Insts.clear();
    collectInsts(Input.Mapping.LHS, Insts);
    // ^ Can this be skipped?
  } while (!Insts.empty());
  return Input;
}

ParsedReplacement Reducer::ReduceRedundantPhis(ParsedReplacement Input) {
  std::set<Inst *> Insts;
  std::vector<Inst *> Phis;

  auto Collect = [&] () {
    Insts.clear();
    Phis.clear();
    collectInsts(Input.Mapping.LHS, Insts);
    collectInsts(Input.Mapping.RHS, Insts);
    for (auto &&I : Insts) {
      if (I->K == Inst::Phi) {
        Phis.push_back(I);
      }
    }
  };
  Collect();

  size_t NumPhis = Phis.size();
  while (NumPhis --) {
    std::map<Inst *, Inst *> ICache;
    for (auto &&I : Phis) {
      if (I->Ops.size() == 1) {
        ICache[I] = I->Ops[0];
      } else if (I->Ops.size() > 1) {
        bool allEq = true;
        for (size_t i = 0; i < I->Ops.size(); ++i) {
          if (I->Ops[i] != I->Ops[0]) {
            allEq = false;
            break;
          }
        }
        if (allEq) {
          ICache[I] = I->Ops[0];
        }
      }
    }

    Input.Mapping.LHS = Replace(Input.Mapping.LHS, IC, ICache);
    Input.Mapping.RHS = Replace(Input.Mapping.RHS, IC, ICache);
    for (auto &PC : Input.PCs) {
      PC.LHS = Replace(PC.LHS, IC, ICache);
      PC.RHS = Replace(PC.RHS, IC, ICache);
    }
    if (NumPhis) {
      Collect();
    }
  }
  return Input;
}

// Assumes Input is valid
ParsedReplacement Reducer::ReducePCs(ParsedReplacement Input) {

  std::set<size_t> UnnecessaryPCs;
  for (size_t i =0; i < Input.PCs.size(); ++i) {
    std::vector<InstMapping> PCsExceptOne;
    for (size_t j = 0; j < Input.PCs.size(); ++j) {
      if (i != j) {
        PCsExceptOne.push_back(Input.PCs[i]);
      }
    }

    std::vector<std::pair<Inst *, llvm::APInt>> Models;
    bool Valid;
    if (std::error_code EC = S->isValid(IC, Input.BPCs, PCsExceptOne, Input.Mapping, Valid, &Models)) {
      llvm::errs() << EC.message() << '\n';
    }
    if (Valid) {
      UnnecessaryPCs.insert(i);
    }
  }

  std::vector<InstMapping> NecessaryPCs;
  for (size_t i =0; i < Input.PCs.size(); ++i) {
    if (UnnecessaryPCs.find(i) == UnnecessaryPCs.end()) {
      NecessaryPCs.push_back(Input.PCs[i]);
    }
  }

  auto Result = Input;
  Result.PCs = NecessaryPCs;
  return Result;
}

// Assumes Input is valid
ParsedReplacement Reducer::WeakenKB(ParsedReplacement Input) {
  std::vector<Inst *> Vars;
  findVars(Input.Mapping.LHS, Vars);
  for (auto &&V : Vars) {
    auto OriZero = V->KnownZeros;
    auto OriOne = V->KnownOnes;
    if (OriZero == 0 && OriOne == 0) {
      continue; // this var doesn't have a knownbits condition
    }
    if (OriZero.getBitWidth() != V->Width  || OriOne.getBitWidth() != V->Width) {
      continue; // this var doesn't have a well formed knownbits condition
    }

    // Try to remove KB
    V->KnownOnes = llvm::APInt(V->Width, 0);
    V->KnownZeros = llvm::APInt(V->Width, 0);
    if (VerifyInput(Input)) {
      continue; // Removed KB from this var
    }
    V->KnownOnes = OriOne;
    V->KnownZeros = OriZero;

    // Try resetting bitwise KB

    for (size_t i = 0; i < V->Width; ++i) {
      auto Ones = V->KnownOnes;
      if (Ones[i]) {
        V->KnownOnes.setBitVal(i, false);
        if (!VerifyInput(Input)) {
          V->KnownOnes = Ones;
        }
      }
      auto Zeros = V->KnownZeros;
      if (Zeros[i]) {
        V->KnownZeros.setBitVal(i, false);
        if (!VerifyInput(Input)) {
          V->KnownZeros = Zeros;
        }
      }
    }
  }
  return Input;
}

// Assumes Input is valid
ParsedReplacement Reducer::WeakenCR(ParsedReplacement Input) {
  // Just try to remove CR for now.
  std::vector<Inst *> Vars;
  findVars(Input.Mapping.LHS, Vars);

  for (auto &&V : Vars) {
    auto Ori = V->Range;
    if (V->Range.isFullSet()) {
      continue;
    }
    V->Range = llvm::ConstantRange(V->Width, true);
    if (!VerifyInput(Input)) {
      V->Range = Ori;
    }
    // TODO: Try Widening Range
  }

  return Input;
}

// Assumes Input is valid
ParsedReplacement Reducer::WeakenDB(ParsedReplacement Input) {
  auto Ori = Input.Mapping.LHS->DemandedBits;
  auto Width = Input.Mapping.LHS->Width;
  if (Ori.getBitWidth() != Width || Ori.isAllOnesValue()) {
    return Input;
  }
  // Try replacing with all ones.
  Input.Mapping.LHS->DemandedBits.setAllBits();
  if (VerifyInput(Input)) {
    return Input;
  }
  Input.Mapping.LHS->DemandedBits = Ori;

  for (size_t i = 0; i < Width; ++i) {
    auto Last = Input.Mapping.LHS->DemandedBits;
    if (!Last[i]) {
      Input.Mapping.LHS->DemandedBits.setBitVal(i, true);
      if (!VerifyInput(Input)) {
        Input.Mapping.LHS->DemandedBits = Last;
      }
    }
  }

  return Input;
}

bool Reducer::VerifyInput(ParsedReplacement &Input) {
  std::vector<std::pair<Inst *, llvm::APInt>> Models;
  bool Valid;
  if (std::error_code EC = S->isValid(IC, Input.BPCs, Input.PCs, Input.Mapping, Valid, &Models)) {
    llvm::errs() << EC.message() << '\n';
  }
  numSolverCalls++;
  return Valid;
}

bool Reducer::safeToRemove(Inst *I, ParsedReplacement &Input) {
  if (I == Input.Mapping.LHS || I == Input.Mapping.RHS || I->K == Inst::Var || I->K == Inst::Const ||
      I->K == Inst::UMulWithOverflow || I->K == Inst::UMulO ||
      I->K == Inst::SMulWithOverflow || I->K == Inst::SMulO ||
      I->K == Inst::UAddWithOverflow || I->K == Inst::UAddO ||
      I->K == Inst::SAddWithOverflow || I->K == Inst::SAddO ||
      I->K == Inst::USubWithOverflow || I->K == Inst::USubO ||
      I->K == Inst::SSubWithOverflow || I->K == Inst::SSubO) {
    return false;
  }
  return true;
}

Inst *Reducer::Eliminate(ParsedReplacement &Input, Inst *I) {
  // Try to replace I with a new Var.
  Inst *NewVar = IC.createVar(I->Width, "newvar" + std::to_string(varnum++));

  std::map<Inst *, Inst *> ICache;
  ICache[I] = NewVar;

  std::map<Block *, Block *> BCache;
  std::map<Inst *, llvm::APInt> CMap;

  ParsedReplacement NewInst = Input;

  Input.Mapping.LHS = getInstCopy(Input.Mapping.LHS, IC, ICache,
                                  BCache, &CMap, false);

  Input.Mapping.RHS = getInstCopy(Input.Mapping.RHS, IC, ICache,
                                  BCache, &CMap, false);

  for (auto &M : Input.PCs) {
    M.LHS = getInstCopy(M.LHS, IC, ICache, BCache, &CMap, false);
    M.RHS = getInstCopy(M.RHS, IC, ICache, BCache, &CMap, false);
  }
  for (auto &BPC : Input.BPCs) {
    BPC.PC.LHS = getInstCopy(BPC.PC.LHS, IC, ICache, BCache, &CMap, false);
    BPC.PC.RHS = getInstCopy(BPC.PC.RHS, IC, ICache, BCache, &CMap, false);
  }
  return NewVar;
}

void Reducer::ReduceRec(ParsedReplacement Input_, std::vector<ParsedReplacement> &Results) {

  // Try to remove subsets of instructions recursively, and store all valid results
  ReplacementContext RC;
  std::string Str;
  llvm::raw_string_ostream SStr(Str);
  RC.printInst(Input_.Mapping.LHS, SStr, false);

  Inst *Ante = IC.getConst(llvm::APInt(1, true));
  for (auto PC : Input_.PCs ) {
    // Inst *Eq = IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
    // Ante = IC.getInst(Inst::And, 1, {Ante, Eq});
    Ante = Builder(PC.LHS, IC).Eq(PC.RHS).And(Ante)();
  }

  RC.printInst(Ante, SStr, false);
  SStr.flush();

//  llvm::errs() << Str << "\n";

//  auto Str = Input_.getString(false);
  if (DNR.find(Str) != DNR.end()) {
    return;
  } else {
    DNR.insert(Str);
  }

  std::set<Inst *> Insts;
  collectInsts(Input_.Mapping.LHS, Insts);
  collectInsts(Input_.Mapping.RHS, Insts);

//  for (auto &&PC : Input_.PCs) {
//    collectInsts(PC.LHS, Insts);
//    collectInsts(PC.RHS, Insts);
//  }

//  for (auto &&BPC : Input_.BPCs) {
//    collectInsts(BPC.PC.LHS, Insts);
//    collectInsts(BPC.PC.RHS, Insts);
//  }

  if (Insts.size() <= 1) {
    return; // Base case
  }

  // Remove at least one instruction and call recursively for valid opts
  for (auto I : Insts) {
    ParsedReplacement Input = Input_;

    if (!safeToRemove(I, Input)) {
      continue;
    }

    Eliminate(Input, I);

    if (VerifyInput(Input)) {
      Results.push_back(Input);
      ReduceRec(Input, Results);
    } else {
      if (DebugLevel >= 2) {
        llvm::outs() << "Invalid attempt.\n";
        Input.print(llvm::outs(), true);
      }
    }
  }
}


}
