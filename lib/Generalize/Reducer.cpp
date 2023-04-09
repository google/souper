#include "llvm/Support/KnownBits.h"
#include "llvm/IR/ConstantRange.h"
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

void collectInstsToDepth(Inst *I, size_t Depth, std::set<Inst *> &Results) {
  std::vector<Inst *> Stack{I};
  std::map<Inst *, size_t> DepthMap;
  DepthMap[I] = 0;
  while (!Stack.empty()) {
    auto Current = Stack.back();
    Stack.pop_back();

    if (DepthMap[Current] > Depth) {
      continue;
    }
    Results.insert(Current);

    for (auto Child : Current->Ops) {
      DepthMap[Child] = DepthMap[Current] + 1;
      if (Results.find(Child) == Results.end()) {
        Stack.push_back(Child);
      }
    }
  }
}

bool IsReductionCostEffective(Inst *LHS, Inst *RHS) {
  return souper::cost(RHS) < souper::cost(LHS);
}

ParsedReplacement Reducer::ReducePairsGreedy(ParsedReplacement Input) {
  size_t Depth = 4, Passes = 5;
  bool Changed = false;
  while (Passes-- ) {
    // Try to remove two instructions at a time
    std::set<Inst *> Insts;

    collectInstsToDepth(Input.Mapping.LHS, Depth, Insts);

    for (auto &&I : Insts) {
      if (!safeToRemove(I, Input)) {
        continue;
      }
      for (auto &&J : Insts) {
        if (I != J) {
          if (!safeToRemove(J, Input)) {
            continue;
          }

          auto Copy = Input;
          Eliminate(Input, I);
          Eliminate(Input, J);

          if (!IsReductionCostEffective(Input.Mapping.LHS, Input.Mapping.RHS)) {
            Input = Copy;
            continue;
          }

          // Input.print(llvm::errs(), true);

          if (!VerifyInput(Input)) {
            Input = Copy;
            continue;
          }
          Changed = true;

        }
      }
    }
    if (!Changed) {
      break;
    }
  }

  return Input;
}

ParsedReplacement Reducer::ReduceTriplesGreedy(ParsedReplacement Input) {
  size_t Depth = 4, Passes = 2;
  bool Changed = false;
  while (Passes-- ) {
    // Try to remove two instructions at a time
    std::set<Inst *> Insts;

    collectInstsToDepth(Input.Mapping.LHS, Depth, Insts);

    for (auto &&I : Insts) {
      if (!safeToRemove(I, Input)) {
        continue;
      }
      for (auto &&J : Insts) {
        if (I != J) {
          if (!safeToRemove(J, Input)) {
            continue;
          }

          for (auto &&K : Insts) {
            if (!safeToRemove(K, Input)) {
              continue;
            }
            if (I != K && J != K) {

              auto Copy = Input;
              Eliminate(Input, I);
              Eliminate(Input, J);
              Eliminate(Input, K);

              if (!IsReductionCostEffective(Input.Mapping.LHS, Input.Mapping.RHS)) {
                Input = Copy;
                continue;
              }

              if (!VerifyInput(Input)) {
                Input = Copy;
                continue;
              }
              Changed = true;
            }
          }
        }
      }
    }
    if (!Changed) {
      break;
    }
  }

  return Input;
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

    if (!IsReductionCostEffective(Input.Mapping.LHS, Input.Mapping.RHS)) {
      Input = Copy;
      failcount++;
      if (failcount >= Insts.size()) {
        break;
      }
      continue;
    }

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

    if (souper::cost(Input.Mapping.LHS) - souper::cost(Input.Mapping.LHS) <= 1) {
      break;
    }

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
//    bool Done = false;
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
        } else {
//          Done = true;
        }
      } else {
//        Done  = true;
      }
    }

//    if (Done || instCount(Input.Mapping.LHS) - instCount(Input.Mapping.RHS) <= 1) {
//      break;
//    }
    if (souper::cost(Input.Mapping.LHS) <= souper::cost(Input.Mapping.RHS)) {
      break;
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
size_t WeakenSingleCR(ParsedReplacement Input, InstContext &IC, Solver *S,
                      Inst *Target, std::optional<llvm::APInt> Val) {
  if (Target->Width <= 8) return 0; // hack
  if (!Val.has_value()) {
    // Synthesize a value
    Inst *C = IC.createVar(Target->Width, "reservedconst_1");
    C->SynthesisConstID = 1;
    std::map<Inst *, Inst *> InstCache = {{Target, C}};

    auto Copy = Input;

    auto Rep = Replace(Input, IC, InstCache);

    std::set<Inst *> ConstSet{C};

    std::map<Inst *, llvm::APInt> ConstMap;
    ConstantSynthesis CS;

//    Rep.print(llvm::errs(), true);

    if (auto EC = CS.synthesize(S->getSMTLIBSolver(), Rep.BPCs,
        Rep.PCs, Rep.Mapping, ConstSet, ConstMap, IC, 30, 60, false)) {
      llvm::errs() << "Constant Synthesis internal error : " <<  EC.message();
    }

    if (!ConstMap.empty()) {
      Val = ConstMap[C];
    }
  }

  if (!Val.has_value()) {
    return 0; // fail
  }

  auto Restore = Target->Range;

  // Binary search to extend upper and lower boundaries
  llvm::ConstantRange R(Val.value());

//  llvm::errs() << "R " << R << " " << Val.value() <<"\n";

  auto Full = R.getFull(R.getBitWidth());

  auto L = R.getLower();
  auto U = R.getUpper();

  size_t inc = 1;
  while (inc && U.slt(Full.getUpper())) {

//    llvm::errs() << "L " << L << " " << "U " << U << " inc " << inc <<"\n";

    auto Backup = Target->Range;
    auto Attempt = U + inc;
    if (Attempt.sge(Full.getUpper())) {
      Attempt = Full.getLower();
    }
    Target->Range = llvm::ConstantRange(L, Attempt);
    if (Verify(Input, IC, S)) {
      U = Attempt;
//      llvm::errs() << "U " << Attempt << '\n';
      inc *= 2;
    } else {
      inc /= 2;
      Target->Range = Backup;
    }
  }

  size_t dec = 1;
  while (dec && L.slt(0)) {
//    llvm::errs() << "L " << L << " " << "U " << U << " inc " << dec <<"\n";
    auto Backup = Target->Range;
    auto Attempt = L - dec;
    if (Attempt.sle(Full.getLower())) {
      Attempt = Full.getLower();
    }
    Target->Range = llvm::ConstantRange(Attempt, U);
    if (Verify(Input, IC, S)) {
      L = Attempt;
//      llvm::errs() << "L " << Attempt << '\n';
      dec *= 2;
    } else {
      dec /= 2;
      Target->Range = Backup;
    }
  }

//  llvm::errs() << "HERE " << L << " " << U << "\n";

  if ((U - L).sgt(1 << (Target->Width - 2))) { // Heuristic
    return (U - L).getLimitedValue();
  } else {
    Target->Range = Restore;
    return 0;
  };

}

size_t WeakenSingleKB(ParsedReplacement Input, InstContext &IC, Solver *S,
                Inst *Target, std::optional<llvm::APInt> Val) {
  size_t BitsWeakened = 0;

  if (Target->Width < 8) return 0; // hack

  if (!Val.has_value()) {
    // Synthesize a value
    Inst *C = IC.createVar(Target->Width, "reservedconst_1");
    C->SynthesisConstID = 1;
    std::map<Inst *, Inst *> InstCache = {{Target, C}};

    auto Copy = Input;

    auto Rep = Replace(Input, IC, InstCache);

    std::set<Inst *> ConstSet{C};

    std::map<Inst *, llvm::APInt> ConstMap;
    ConstantSynthesis CS;

//    Rep.print(llvm::errs(), true);

    if (auto EC = CS.synthesize(S->getSMTLIBSolver(), Rep.BPCs,
        Rep.PCs, Rep.Mapping, ConstSet, ConstMap, IC, 30, 60, false)) {
      llvm::errs() << "Constant Synthesis internal error : " <<  EC.message();
    }

    if (!ConstMap.empty()) {
      Val = ConstMap[C];
    }
  }

  if (!Val.has_value()) {
    return 0; // No bits weakened
  }

  llvm::APInt RestoreZero = Target->KnownZeros;
  llvm::APInt RestoreOne = Target->KnownOnes;

  Target->KnownOnes = Val.value();
  Target->KnownZeros = ~Val.value();

  for (size_t i = 0; i < Target->Width; ++i) {
    llvm::APInt OriZ = Target->KnownZeros;
    llvm::APInt OriO = Target->KnownOnes;

    if (OriO[i] == 0 && OriZ[i] == 0) {
      continue;
    }

    if (OriO[i] == 1) Target->KnownOnes.clearBit(i);
    if (OriZ[i] == 1) Target->KnownZeros.clearBit(i);

    if (!Verify(Input, IC, S)) {
      Target->KnownZeros = OriZ;
      Target->KnownOnes = OriO;
    } else {
      BitsWeakened++;
    }
  }

  if (BitsWeakened < Target->Width / 2) {
    Target->KnownOnes = RestoreOne;
    Target->KnownZeros = RestoreZero;
    BitsWeakened = 0;
  }

  return BitsWeakened;
}

ParsedReplacement Reducer::ReducePCsToDF(ParsedReplacement Input) {
  std::vector<Inst *> FoundVars;
  for (auto &&PC : Input.PCs) {
    findVars(PC.LHS, FoundVars);
    findVars(PC.RHS, FoundVars);
  }
  std::set<Inst *> Vars;
  for (auto &&V : FoundVars) {
    if (!V->Name.starts_with("sym") && !V->Name.starts_with("const")) {
      Vars.insert(V);
    }
  }

  auto Backup = Input.PCs;
  Input.PCs.clear();

  bool Succ = false;

  for (auto &&V : Vars) {
    auto RangeSize = WeakenSingleCR(Input, IC, S, V, {});
    Succ |= (RangeSize > 0);
  }

  if (!Succ) {
    for (auto &&V : Vars) {
      auto BitsWeakened = WeakenSingleKB(Input, IC, S, V, {});
      Succ |= (BitsWeakened != 0);
    }
  }

  if (!Succ) {
    Input.PCs = Backup;
  }

  return Input;
}

// Assumes Input is valid
ParsedReplacement Reducer::ReducePCs(ParsedReplacement Input) {

  for (size_t i = 0; i < Input.PCs.size(); ++i) {
    auto Result = Input;
    Result.PCs.clear();
    for (size_t j = 0; j < Input.PCs.size(); ++j) {
      if (i != j) {
        Result.PCs.push_back(Input.PCs[j]);
      }
    }

    auto Clone = Verify(Result, IC, S);
    if (Clone) {
      return ReducePCs(Result);
    }
  }

  return Input;
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

    auto R = V->Range;

    if (!R.isWrappedSet()) {
      auto Full = R.getFull(R.getBitWidth());

      auto L = R.getLower();
      auto U = R.getUpper();

      size_t inc = 1;
      while (inc && U.slt(Full.getUpper())) {
        auto Backup = V->Range;
        auto Attempt = U + inc;

        if (Attempt.sge(Full.getUpper())) {
          Attempt = Full.getLower();
        }

        V->Range = llvm::ConstantRange(L, Attempt);
        if (VerifyInput(Input)) {
          U = Attempt;
    //      llvm::errs() << "U " << Attempt << '\n';
          inc *= 2;
        } else {
          inc /= 2;
          V->Range = Backup;
        }
      }

      size_t dec = 1;
      while (dec && L.slt(0)) {
        auto Backup = V->Range;
        auto Attempt = L - dec;
        if (Attempt.sle(Full.getLower())) {
          Attempt = Full.getLower();
        }
        V->Range = llvm::ConstantRange(Attempt, U);
        if (VerifyInput(Input)) {
          L = Attempt;
    //      llvm::errs() << "L " << Attempt << '\n';
          dec *= 2;
        } else {
          dec /= 2;
          V->Range = Backup;
        }
      }
    }
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

// Assumes Input is valid
ParsedReplacement Reducer::WeakenOther(ParsedReplacement Input) {
  std::vector<Inst *> Vars;
  findVars(Input.Mapping.LHS, Vars);

  for (auto &&V : Vars) {
#define WEAKEN(X)           \
if (V->X) {                 \
  V->X = false;             \
  if (!VerifyInput(Input)) {\
    V->X = true;}}

    WEAKEN(NonZero)
    WEAKEN(NonNegative)
    WEAKEN(PowOfTwo)
    WEAKEN(Negative)

#undef WEAKEN

    while (V->NumSignBits) {
      V->NumSignBits--;
      if (!VerifyInput(Input)) {
        V->NumSignBits++;
        break;
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
  if (I == Input.Mapping.LHS || I->K == Inst::Var || I->K == Inst::Const ||
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

  if (souper::cost(Input_.Mapping.LHS) - souper::cost(Input_.Mapping.LHS) <= 1) {
    return;
  }

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

Inst *NonPoisonReplacement(Inst *I, InstContext &IC) {
  Inst::Kind K = I->K;
  switch (I->K) {
    case Inst::AddNW:
    case Inst::AddNUW:
    case Inst::AddNSW:
      K = Inst::Add;
      break;
    case Inst::SubNW:
    case Inst::SubNUW:
    case Inst::SubNSW:
      K = Inst::Sub;
      break;
    case Inst::MulNW:
    case Inst::MulNUW:
    case Inst::MulNSW:
      K = Inst::Mul;
      break;
    case Inst::ShlNW:
    case Inst::ShlNUW:
    case Inst::ShlNSW:
      K = Inst::Shl;
      break;
    case Inst::UDivExact:
      K = Inst::UDiv;
      break;
    case Inst::SDivExact:
      K = Inst::SDiv;
      break;
    default:
      llvm_unreachable("Expected instruction with poison flag.");
  }

  auto Ret =  IC.getInst(K, I->Width, I->Ops);
  Ret->Name = I->Name;
  Ret->DemandedBits = I->DemandedBits;
  return Ret;
}

void CollectPoisonInsts(Inst *I, std::set<Inst *> &PoisonInsts,
                        std::set<Inst *> &Visited) {
  if (Visited.find(I) != Visited.end()) {
    return;
  }
  Visited.insert(I);

  if (I->K == Inst::AddNSW || I->K == Inst::AddNUW || I->K == Inst::AddNW ||
      I->K == Inst::SubNSW || I->K == Inst::SubNUW || I->K == Inst::SubNW ||
      I->K == Inst::MulNSW || I->K == Inst::MulNUW || I->K == Inst::MulNW ||
      I->K == Inst::ShlNSW || I->K == Inst::ShlNUW || I->K == Inst::ShlNW ||
      I->K == Inst::UDivExact || I->K == Inst::SDivExact) {
    PoisonInsts.insert(I);
  }

  for (auto &&Op : I->Ops) {
    CollectPoisonInsts(Op, PoisonInsts, Visited);
  }
}

ParsedReplacement Reducer::ReducePoison(ParsedReplacement Input) {
  std::set<Inst *> PoisonInsts;
  std::set<Inst *> Visited;
  CollectPoisonInsts(Input.Mapping.LHS, PoisonInsts, Visited);
  CollectPoisonInsts(Input.Mapping.RHS, PoisonInsts, Visited);
  for (auto &&PC : Input.PCs) {
    CollectPoisonInsts(PC.LHS, PoisonInsts, Visited);
    CollectPoisonInsts(PC.RHS, PoisonInsts, Visited);
  }

  for (auto I : PoisonInsts) {
    auto Rep = NonPoisonReplacement(I, IC);
    if (!Rep) {
      continue;
    }
    std::map<Inst *, Inst *> Cache = {{I, NonPoisonReplacement(I, IC)}};

    auto Cand = Replace(Input, IC, Cache);

    if (VerifyInput(Cand)) {
      Input = Cand;
    }
  }

  return Input;
}


}
