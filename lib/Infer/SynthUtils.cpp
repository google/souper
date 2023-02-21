#include "souper/Infer/SynthUtils.h"
#include "souper/Infer/Pruning.h"

namespace souper {

Inst *Replace(Inst *R, InstContext &IC, std::map<Inst *, Inst *> &M) {
  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, llvm::APInt> ConstMap;
  return getInstCopy(R, IC, M, BlockCache, &ConstMap, false);
}

ParsedReplacement Replace(ParsedReplacement I, InstContext &IC,
                          std::map<Inst *, Inst *> &M) {
  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, llvm::APInt> ConstMap;
  
  I.Mapping.LHS =  getInstCopy(I.Mapping.LHS, IC, M, BlockCache, &ConstMap, false);
  I.Mapping.RHS =  getInstCopy(I.Mapping.RHS, IC, M, BlockCache, &ConstMap, false);
  
  for (auto &PC : I.PCs) {
    PC.LHS = getInstCopy(PC.LHS, IC, M, BlockCache, &ConstMap, false, false);
    PC.RHS = getInstCopy(PC.RHS, IC, M, BlockCache, &ConstMap, false, false);
  }
  
  return I;
}

Inst *Clone(Inst *R, InstContext &IC) {
  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, llvm::APInt> ConstMap;
  std::map<Inst *, Inst *> InstCache;
  return getInstCopy(R, IC, InstCache, BlockCache, &ConstMap, true, false);
}

InstMapping Clone(InstMapping In, InstContext &IC) {
  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, llvm::APInt> ConstMap;
  std::map<Inst *, Inst *> InstCache;
  InstMapping Out;
  Out.LHS = getInstCopy(In.LHS, IC, InstCache, BlockCache, &ConstMap, true, false);
  Out.RHS = getInstCopy(In.RHS, IC, InstCache, BlockCache, &ConstMap, true, false);
  return Out;
}

ParsedReplacement Clone(ParsedReplacement In, InstContext &IC) {
  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, llvm::APInt> ConstMap;
  std::map<Inst *, Inst *> InstCache;
  std::vector<Inst *> RHSVars;
  findVars(In.Mapping.RHS, RHSVars);
  In.Mapping.LHS = getInstCopy(In.Mapping.LHS, IC, InstCache, BlockCache, &ConstMap, true, false);
  In.Mapping.RHS = getInstCopy(In.Mapping.RHS, IC, InstCache, BlockCache, &ConstMap, true, false);
  for (auto &PC : In.PCs) {
    PC.LHS = getInstCopy(PC.LHS, IC, InstCache, BlockCache, &ConstMap, false, false);
    PC.RHS = getInstCopy(PC.RHS, IC, InstCache, BlockCache, &ConstMap, false, false);
  }

  for (auto &V : RHSVars) {
    if (V->SymOneOf) {
      InstCache[V->SymOneOf]->SymKnownOnes = InstCache[V];
    }
    if (V->SymZeroOf) {
      InstCache[V->SymZeroOf]->SymKnownZeros = InstCache[V];
    }
  }
  
  return In;
}

//std::map <Inst *, llvm::APInt> ConstantSynthesis

// Also Synthesizes given constants
// Returns clone if verified, nullptrs if not
ParsedReplacement Verify(ParsedReplacement Input, InstContext &IC, Solver *S) {
  SynthesisContext SC{IC, S->getSMTLIBSolver(), Input.Mapping.LHS, nullptr,
              Input.PCs,Input.BPCs, false, 15};
  std::vector<Inst *> Vars;
  findVars(Input.Mapping.LHS, Vars);

//  PruningManager Pruner(SC, Vars, 5);
//  Pruner.init();
//
//  if (Pruner.isInfeasible(Input.Mapping.RHS, 0)) {
//    Input.Mapping.LHS = nullptr;
//    Input.Mapping.RHS = nullptr;
//    return Input;
//  }

  Input = Clone(Input, IC);
  std::set<Inst *> ConstSet;
  souper::getConstants(Input.Mapping.RHS, ConstSet);
  if (!ConstSet.empty()) {
    std::map <Inst *, llvm::APInt> ResultConstMap;
    ConstantSynthesis CS;
    auto SMTSolver = S->getSMTLIBSolver();
    
    auto EC = CS.synthesize(SMTSolver, Input.BPCs, Input.PCs,
                         Input.Mapping, ConstSet,
                         ResultConstMap, IC, /*MaxTries=*/30, 10,
                         /*AvoidNops=*/true);
    if (!ResultConstMap.empty()) {
      std::map<Inst *, Inst *> InstCache;
      std::map<Block *, Block *> BlockCache;
      auto LHSCopy = getInstCopy(Input.Mapping.LHS, IC, InstCache, BlockCache, &ResultConstMap, true);
      auto RHS = getInstCopy(Input.Mapping.RHS, IC, InstCache, BlockCache, &ResultConstMap, true);
      Input.Mapping = InstMapping(LHSCopy, RHS);
      for (auto &PC : Input.PCs) {
        PC.LHS = getInstCopy(PC.LHS, IC, InstCache, BlockCache, &ResultConstMap, true);
        PC.RHS = getInstCopy(PC.RHS, IC, InstCache, BlockCache, &ResultConstMap, true);
      }
      return Input;
    } else {
      if (DebugLevel > 2) {
        llvm::errs() << "Constant Synthesis ((no Dataflow Preconditions)) failed. \n";
      }
    }
    Input.Mapping = InstMapping(nullptr, nullptr);
    return Input;
  }
  std::vector<std::pair<Inst *, llvm::APInt>> Models;
  bool IsValid;
  if (auto EC = S->isValid(IC, Input.BPCs, Input.PCs, Input.Mapping, IsValid, &Models)) {
    llvm::errs() << EC.message() << '\n';
  }
  if (IsValid) {
    return Input;
  } else {
    static int C = 0;
//    llvm::errs() << "C " << C++ << '\n';
    Input.Mapping = InstMapping(nullptr, nullptr);
    return Input;
    // TODO: Better failure indication?
  }
}

std::map<Inst *, llvm::APInt> findOneConstSet(ParsedReplacement Input, const std::set<Inst *> &SymCS, InstContext &IC, Solver *S) {
  
  std::map<Inst *, Inst *> InstCache;
  
  std::set<Inst *> SynthCS;
  
  size_t cid = 0;
  for (auto C : SymCS) {
    InstCache[C] = IC.createSynthesisConstant(C->Width, cid++);
    SynthCS.insert(InstCache[C]);
  }
  Input = Replace(Input, IC, InstCache);
  
  std::map <Inst *, llvm::APInt> ResultConstMap;
  ConstantSynthesis CS;
  auto EC = CS.synthesize(S->getSMTLIBSolver(), Input.BPCs, Input.PCs,
                       Input.Mapping, SynthCS,
                       ResultConstMap, IC, /*MaxTries=*/30, 10,
                       /*AvoidNops=*/true);
  
  std::map<Inst *, llvm::APInt> Result;
  if (!ResultConstMap.empty()) {
    for (auto C : SymCS) {
      Result[C] = ResultConstMap[InstCache[C]];
    }
  }
  
  return Result;
  
}

std::vector<std::map<Inst *, llvm::APInt>> findValidConsts(ParsedReplacement Input, const std::set<Inst *> &Insts, InstContext &IC, Solver *S, size_t MaxCount = 1) {
  
  // FIXME: Ignores Count
  std::vector<std::map<Inst *, llvm::APInt>> Results;
  
  Inst *T = IC.getConst(llvm::APInt(1, 1)); // true
  Inst *F = IC.getConst(llvm::APInt(1, 0)); // false
  
  while (MaxCount-- ) {
    auto &&Result = findOneConstSet(Input, Insts, IC, S);
    if (Result.empty()) {
      break;
    } else {
      Results.push_back(Result);
      for (auto I : Result) {
        Input.PCs.push_back({Builder(I.first, IC).Ne(I.second)(), T});
      }
    }
  }
  
  return Results;
}

}