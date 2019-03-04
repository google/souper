#include "souper/Infer/DataflowPruning.h"
#include <cstdlib>
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

// TODO : Comment out debug stmts and conditions before benchmarking
bool ValueAnalysis::isInfeasible(souper::Inst *RHS,
                                 unsigned StatsLevel) {
  for (int I = 0; I < Inputs.size(); ++I) {
    auto C = LHSValues[I];
    if (C.hasValue()) {
      auto Val = C.getValue();
      if (StatsLevel > 2) {
        llvm::errs() << "  Input:\n";
        for (auto &&p : Inputs[I]) {
          if (p.second.hasValue()) {
            llvm::errs() << "  Var " << p.first->Name << " : "
                         << p.second.getValue() << "\n";
          }
        }
        llvm::errs() << "  LHS value = " << Val << "\n";
      }

      if (!isConcrete(RHS)) {
        auto CR = findConstantRange(RHS, Inputs[I]);
        if (StatsLevel > 2)
          llvm::errs() << "  RHS ConstantRange = " << CR << "\n";
        if (!CR.contains(Val)) {
          if (StatsLevel > 2) {
            llvm::errs() << "  pruned using CR! ";
            if (!isConcrete(RHS, false, true)) {
              llvm::errs() << "Inst had a hole.";
            } else {
              llvm::errs() << "Inst had a symbolic const.";
            }
            llvm::errs() << "\n";
          }
          return true;
        }
        auto KB = findKnownBits(RHS, Inputs[I]);
        if (StatsLevel > 2)
          llvm::errs() << "  RHS KnownBits = " << knownBitsString(KB) << "\n";
        if ((KB.Zero & Val) != 0 || (KB.One & ~Val) != 0) {
          if (StatsLevel > 2) {
            llvm::errs() << "  pruned using KB! ";
            if (!isConcrete(RHS, false, true)) {
              llvm::errs() << "Inst had a hole.";
            } else {
              llvm::errs() << "Inst had a symbolic const.";
            }
            llvm::errs() << "\n";
          }
          return true;
        }
      } else {
        auto RHSV = evaluateInst(RHS, Inputs[I]);
        if (RHSV.hasValue()) {
          if (Val != RHSV.getValue()) {
            if (StatsLevel > 2) {
              llvm::errs() << "  RHS value = " << RHSV.getValue() << "\n";
              llvm::errs() << "  pruned using concrete interpreter!\n";
            }
            return true;
          }
        }
      }
    }
  }
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
      ReplacementContext RC;
      RC.printInst(I, llvm::errs(), true);
      if (VA.isInfeasible(I, StatsLevel)) {
        NumPruned++;
        llvm::errs() << "Tally: "
          << NumPruned << "/" << TotalGuesses << "\n";
        llvm::errs() << "Pruned." << Inst::getKindName(I->K) << "\n\n";
        return false;
      }
      llvm::errs() << "Could not prune." << Inst::getKindName(I->K) << "\n\n";
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

// FIXME: Only generate inputs which obey PC
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

  for (auto &&I : Inputs) {
    if (I->K == souper::Inst::Var)
      Cache[I] = {llvm::APInt(I->Width, 1)};
  }
  InputSets.push_back(Cache);

  for (auto &&I : Inputs) {
    if (I->K == souper::Inst::Var)
      Cache[I] = {llvm::APInt(I->Width, -1)};
  }
  InputSets.push_back(Cache);

  for (auto &&I : Inputs) {
    if (I->K == souper::Inst::Var)
      Cache[I] = {llvm::APInt(I->Width, 0xFFF)};
  }
  InputSets.push_back(Cache);

  constexpr int NumLargeInputs = 5;
  std::srand(0);
  for (int i = 0 ; i < NumLargeInputs; ++i ) {
    for (auto &&I : Inputs) {
      if (I->K == souper::Inst::Var)
        Cache[I] = {llvm::APInt(I->Width, std::rand() % llvm::APInt(I->Width, -1).getLimitedValue())};
    }
    InputSets.push_back(Cache);
  }

  constexpr int NumSmallInputs = 5;
  for (int i = 0 ; i < NumSmallInputs; ++i ) {
    for (auto &&I : Inputs) {
      if (I->K == souper::Inst::Var)
        Cache[I] = {llvm::APInt(I->Width, std::rand() % I->Width)};
    }
    InputSets.push_back(Cache);
  }

  return InputSets;
}

}
