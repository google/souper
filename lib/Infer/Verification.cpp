#include "souper/Infer/Verification.h"
#include "souper/Extractor/ExprBuilder.h"
namespace souper {
void collectPhis(souper::Inst *I, std::map<souper::Block *, std::set<souper::Inst *>> &Phis) {
  std::vector<Inst *> Stack{I};
  std::unordered_set<Inst *> Visited;
  while (!Stack.empty()) {
    auto Current = Stack.back();
    Stack.pop_back();
    if (Current->K == Inst::Phi) {
      Phis[Current->B].insert(Current);
    }
    Visited.insert(Current);
    for (auto Child : Current->Ops) {
      if (Visited.find(Child) == Visited.end()) {
        Stack.push_back(Child);
      }
    }
  }
}

std::unordered_set<RefinementProblem, RefinementProblem::Hash>
  explodePhis(InstContext &IC, RefinementProblem P) {
  std::map<souper::Block *, std::set<souper::Inst *>> Phis;
  collectPhis(P.LHS, Phis);
  collectPhis(P.Pre, Phis);

  if (Phis.empty()) {
    return {P};
  }

  std::vector<Block *> Blocks;
  for (auto &&Pair : Phis) {
    Blocks.push_back(Pair.first);
  }

  std::vector<std::map<Block *, size_t>> ChangeList;

  for (size_t i = 0; i < Blocks.size(); ++i) { // Each block
    if (i == 0) {
      for (size_t j = 0; j < Blocks[i]->Preds; ++j) {
        ChangeList.push_back({{Blocks[i], j}});
      }
    } else {
      std::vector<std::map<Block *, size_t>> NewChangeList;
      for (size_t j = 0; j < Blocks[i]->Preds; ++j) {
        for (auto Change : ChangeList) {
          Change.insert({Blocks[i], j});
          NewChangeList.push_back(Change);
        }
      }
      std::swap(ChangeList, NewChangeList);
    }
  }

  std::unordered_set<RefinementProblem, RefinementProblem::Hash> Result;

  for (auto Change : ChangeList) {
    auto Goal = P.ReplacePhi(IC, Change);
    // Consider switching to better data structures for dealing with BPCs
    for (auto &[Block, Pred] : Change) {
      for (auto &BPC : Goal.BPCs) {
        if (BPC.B == Block && BPC.PredIdx == Pred) {
          auto Ante = IC.getInst(Inst::Eq, 1, {BPC.PC.LHS, BPC.PC.RHS});
          Goal.Pre = IC.getInst(Inst::And, 1, {Goal.Pre, Ante});
        }
      }
    }
    Goal.LHS->DemandedBits = P.LHS->DemandedBits;
    Result.insert(Goal);
  }
  return Result;
}

Inst *getDataflowConditions(const Inst *I, InstContext &IC) {
  // Dummy because it doesn't actually build expressions.
  // It exists for the purpose of reusing parts of the abstract ExprBuilder here.
  // FIXME: Allow creating objects of ExprBuilder
  class DummyExprBuilder : public souper::ExprBuilder {
  public:
    DummyExprBuilder(souper::InstContext &IC) : souper::ExprBuilder(IC) {}
    std::string BuildQuery(const souper::BlockPCs & BPCs,
                           const std::vector<souper::InstMapping> & PCs,
                           souper::InstMapping Mapping,
                           std::vector<souper::Inst *> * ModelVars,
                           souper::Inst *Precondition,
                           bool Negate, bool DropUB) override {
      llvm::report_fatal_error("Do not call");
      return "";
    }
    std::string GetExprStr(const souper::BlockPCs & BPCs,
                           const std::vector<souper::InstMapping> & PCs,
                           souper::InstMapping Mapping,
                           std::vector<souper::Inst *> * ModelVars,
                           bool Negate, bool DropUB) override {
      llvm::report_fatal_error("Do not call");
      return "";
    }
  };
  DummyExprBuilder EB(IC);
  return EB.getDataflowConditions(const_cast<Inst *>(I));

}

RefinementProblem RefinementProblem::ReplacePhi(souper::InstContext &IC, std::map<Block *, size_t> &Change) {
  std::map<souper::Block *, std::set<souper::Inst *>> Phis;
  collectPhis(LHS, Phis);
  collectPhis(RHS, Phis);
  collectPhis(Pre, Phis);
  for (auto &BPC : BPCs) {
    collectPhis(BPC.PC.LHS, Phis);
  }

  if (Phis.empty()) {
    return *this; // Base case, no more Phis
  }

  std::map<Inst *, Inst *> InstCache;
  for (auto Pair : Phis) {
    for (auto Phi : Pair.second) {
      InstCache[Phi] = Phi->Ops[Change[Pair.first]];
    }
  }
  std::map<Block *, Block *> BlockCache;
  std::map<Inst *, llvm::APInt> ConstMap;
  RefinementProblem Result;
  Result.LHS = getInstCopy(LHS, IC, InstCache, BlockCache, &ConstMap, false);
  Result.RHS = getInstCopy(RHS, IC, InstCache, BlockCache, &ConstMap, false);
  Result.Pre = getInstCopy(Pre, IC, InstCache, BlockCache, &ConstMap, false);
  Result.BPCs = BPCs;
  for (auto &BPC : Result.BPCs) {
    BPC.PC.LHS = getInstCopy(BPC.PC.LHS, IC, InstCache, BlockCache,
                             &ConstMap, false);
  }

  // Recursively call ReplacePhi, because Result might have Phi`s
  return Result.ReplacePhi(IC, Change);
}

}
