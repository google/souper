#include "souper/Infer/DataflowPruning.h"
namespace souper {

std::string knownBitsString(llvm::KnownBits KB) {
  std::string S = "";
  for (int I = 0; I < KB.getBitWidth(); I++) {
    if (KB.Zero.isNegative())
      S += "0";
    else if (KB.One.isNegative())
      S += "1";
    else
      S += "?";
    KB.Zero <<= 1;
    KB.One <<= 1;
  }
  return S;
}

bool ValueAnalysis::isInfeasible(souper::Inst *RHS,
				 unsigned StatsLevel) {
  for (int I = 0; I < Inputs.size(); ++I) {
    auto C = LHSValues[I];
    if (C.hasValue()) {
      auto Val = C.getValue();
      if (StatsLevel > 2)
	llvm::errs() << "  LHS value = " << Val << "\n";
      auto CR = findConstantRange(RHS, Inputs[I]);
      if (StatsLevel > 2)
	llvm::errs() << "  RHS ConstantRange = " << CR << "\n";
      if (!CR.contains(Val)) {
	if (StatsLevel > 2)
	  llvm::errs() << "  pruned using CR!\n";
        return true;
      }
      auto KB = findKnownBits(RHS, Inputs[I]);
      if (StatsLevel > 2)
	llvm::errs() << "  RHS KnownBits = " << knownBitsString(KB) << "\n";
      if ((KB.Zero & Val) != 0 || (KB.One & ~Val) != 0) {
	if (StatsLevel > 2)
	  llvm::errs() << "  pruned using KB!\n";
        return true;
      }

      auto RHSV = evaluateInst(RHS, Inputs[I]);
      if (RHSV.hasValue()) {
        if (Val != RHSV.getValue()) {
	  if (StatsLevel > 2)
	    llvm::errs() << "  pruned using interpreter!\n";
          return true;
        }
      }
    }
  }
  if (StatsLevel > 2)
    llvm::errs() << "  could not prune.\n";
  return false;
}
DataflowPruningManager::DataflowPruningManager(
  souper::Inst* LHS_, std::vector<Inst *> &Inputs_, unsigned StatsLevel_)
  : LHS(LHS_), Inputs(Inputs_), StatsLevel(StatsLevel_), NumPruned(0),
    TotalGuesses(0) {}

void DataflowPruningManager::init() {
  VA = ValueAnalysis(LHS, generateInputSets(Inputs));
  if (StatsLevel > 1) {
    DataflowPrune= [this](Inst *I, std::vector<Inst *> &RI) {
      TotalGuesses++;
      if (VA.isInfeasible(I, StatsLevel)) {
        NumPruned++;
        llvm::errs() << "Dataflow Pruned "
          << NumPruned << "/" << TotalGuesses << "\n";
        ReplacementContext RC;
        RC.printInst(I, llvm::errs(), true);
        return false;
      }
      return true;
    };
  } else if (StatsLevel == 1) {
      DataflowPrune= [this](Inst *I, std::vector<Inst *> &RI) {
      TotalGuesses++;
      if (VA.isInfeasible(I, StatsLevel)) {
        NumPruned++;
        return false;
      }
      return true;
    };
  } else {
    DataflowPrune= [this](Inst *I, std::vector<Inst *> &RI) {
      return !VA.isInfeasible(I, StatsLevel);
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
