#include "souper/Infer/DataflowPruning.h"
namespace souper {

bool ValueAnalysis::isInfeasible(souper::Inst* RHS) {
  for (int I = 0; I < Inputs.size(); ++I) {
    auto C = LHSValues[I];
    if (C.hasValue()) {
      auto CR = findConstantRange(RHS, Inputs[I]);
      if (!CR.contains(C.getValue())) {
        return true;
      }
      auto KB = findKnownBits(RHS, Inputs[I]);
      if ((KB.Zero & C.getValue()) != 0 || (KB.One & ~C.getValue()) != 0) {
        return true;
      }

      auto RHSV = evaluateInst(RHS, Inputs[I]);
      if (RHSV.hasValue()) {
        if (C.getValue() != RHSV.getValue()) {
          return true;
        }
      }
    }
  }
  return false;
}
DataflowPruningManager::DataflowPruningManager(
  souper::Inst* LHS, std::vector<Inst *> &Inputs, unsigned StatsLevel)
  : VA(LHS, generateInputSets(Inputs)), NumPruned(0), TotalGuesses(0) {
  if (StatsLevel > 1) {
    DataflowPrune= [this](Inst *I, std::vector<Inst *> &RI) {
      TotalGuesses++;
      if (VA.isInfeasible(I)) {
        NumPruned++;
        llvm::outs() << "Dataflow Pruned "
          << NumPruned << "/" << TotalGuesses << "\n";
        ReplacementContext RC;
        RC.printInst(I, llvm::outs(), true);
        return false;
      }
      return true;
    };
  } else if (StatsLevel == 1) {
      DataflowPrune= [this](Inst *I, std::vector<Inst *> &RI) {
      TotalGuesses++;
      if (VA.isInfeasible(I)) {
        NumPruned++;
        return false;
      }
      return true;
    };
  } else {
    DataflowPrune= [this](Inst *I, std::vector<Inst *> &RI) {
      return !VA.isInfeasible(I);
    };
  }
}

std::vector<ValueCache> DataflowPruningManager::generateInputSets(
  std::vector<Inst *> &Inputs) {
  std::vector<ValueCache> InputSets;

  ValueCache Cache;
  int64_t Current = 0;
  for (auto &&I : Inputs) {
    if (I->K == souper::Inst::Var)
      Cache[I] = {llvm::APInt(I->Width, Current++)};
  }
  InputSets.push_back(Cache);

  Current = 2*Current + 1;
  for (auto &&I : Inputs) {
    if (I->K == souper::Inst::Var)
      Cache[I] = {llvm::APInt(I->Width, Current++)};
  }
  InputSets.push_back(Cache);

  return InputSets;
}

}
