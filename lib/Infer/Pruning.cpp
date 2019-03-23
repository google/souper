// Copyright 2019 The Souper Authors. All rights reserved.
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

#include "souper/Infer/Pruning.h"
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

std::string getUniqueName() {
  static int counter = 0;
  return "dummy" + std::to_string(counter++);
}

// TODO : Comment out debug stmts and conditions before benchmarking
bool PruningManager::isInfeasible(souper::Inst *RHS,
                                 unsigned StatsLevel) {
  for (int I = 0; I < InputVals.size(); ++I) {
    auto C = LHSValues[I];
    if (C.hasValue()) {
      auto Val = C.getValue();
      if (StatsLevel > 2) {
        llvm::errs() << "  Input:\n";
        for (auto &&p : InputVals[I]) {
          if (p.second.hasValue()) {
            llvm::errs() << "  Var " << p.first->Name << " : "
                         << p.second.getValue() << "\n";
          }
        }
        llvm::errs() << "  LHS value = " << Val << "\n";
      }

      if (!isConcrete(RHS)) {
        auto CR = findConstantRange(RHS, InputVals[I]);
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
        auto KB = findKnownBits(RHS, InputVals[I]);
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
        auto RHSV = evaluateInst(RHS, InputVals[I]);
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
  return isInfeasibleWithSolver(RHS, StatsLevel);
}

bool PruningManager::isInfeasibleWithSolver(Inst *RHS, unsigned StatsLevel) {
  for (int I = 0; I < InputVals.size(); ++I) {
    auto C = LHSValues[I];
    if (C.hasValue()) {
      auto Val = C.getValue();
      if (!isConcrete(RHS, false, true)) {
        std::vector<Inst *> Holes, ModelVars;
        getReservedInsts(RHS, Holes);
        std::map<Inst *, Inst *> InstCache;
        std::vector<Inst *> Empty;
        for (auto *Hole : Holes) {
          auto DummyVar = IC.createVar(Hole->Width, getUniqueName());
          InstCache[Hole] = DummyVar;
        }
        std::map<Inst *, llvm::APInt> ConstMap;
        for (auto P : InputVals[I]) {
          if (P.second.hasValue()) {
            ConstMap[P.first] = P.second.getValue();
          } else {
            continue;
          }
        }
        std::map<Block *, Block *> BlockCache;
        auto RHSReplacement = getInstCopy(RHS, IC, InstCache, BlockCache, &ConstMap, true);
        auto LHSReplacement = IC.getConst(Val);

        auto Cond = IC.getInst(Inst::Eq, 1, {LHSReplacement, RHSReplacement});
        InstMapping Mapping {Cond, IC.getConst(llvm::APInt(1, true))};
        auto Query = BuildQuery(IC, {}, {}, Mapping, &ModelVars, true);
        if (StatsLevel > 3) {
          llvm::errs() << Query << "\n";

          llvm::errs() << "LHS\n";
          ReplacementContext RC1; RC1.printInst(Mapping.LHS, llvm::errs(), true);
          llvm::errs() << "RHS\n";
          ReplacementContext RC2; RC2.printInst(Mapping.RHS, llvm::errs(), true);
        }

        bool Result;
        std::vector<llvm::APInt> Models(ModelVars.size());
        auto EC = SMTSolver->isSatisfiable(Query, Result, Models.size(), &Models, 1000);

        if (EC) {
          llvm::errs() << "Solver error in Pruning. " << EC.message() << " \n";
          continue;
        }
        if (!Result) {
          if (StatsLevel > 2) {
            llvm::errs() << "  pruned using Solver! ";
            if (!isConcrete(RHS, false, true)) {
              llvm::errs() << "Inst had a hole.";
            } else {
              llvm::errs() << "Inst had a symbolic const.";
            }
            llvm::errs() << "\n";
          }
          return true;
        } else {
          if (StatsLevel > 2) {
            llvm::errs() << "Failed to prune using Solver, Solver returned SAT\n";
            llvm::errs() << "Model:";
            for (int i = 0; i < Holes.size(); ++i) {
              llvm::errs() << ModelVars[i]->Name << " : " << Models[i] << "\n";
            }
            llvm::errs() << "\n\n";
          }
        }

      }
    }
  }
  return false;
}

PruningManager::PruningManager(
  souper::Inst *LHS_, std::vector<Inst *> &Inputs_, unsigned StatsLevel_,
  InstContext &IC_, SMTLIBSolver *SMTSolver_)
                  : LHS(LHS_), IC(IC_), NumPruned(0),
                    TotalGuesses(0),
                    StatsLevel(StatsLevel_), SMTSolver(SMTSolver_),
                    InputVars(Inputs_) {}

void PruningManager::init() {

  InputVals = generateInputSets(InputVars);

  for (auto &&Input : InputVals) {
    LHSValues.push_back(evaluateInst(LHS, Input));
  }

  if (StatsLevel > 1) {
    DataflowPrune= [this](Inst *I, std::vector<Inst *> &RI) {
      TotalGuesses++;
      ReplacementContext RC;
      RC.printInst(I, llvm::errs(), true);
      if (isInfeasible(I, StatsLevel)) {
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
      if (isInfeasible(I, StatsLevel)) {
        NumPruned++;
        return false;
      }
      return true;
    };
  } else {
    DataflowPrune= [this](Inst *I, std::vector<Inst *> &RI) {
      return !isInfeasible(I, StatsLevel);
    };
  }
}

// FIXME: Only generate inputs which obey PC
std::vector<ValueCache> PruningManager::generateInputSets(
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
