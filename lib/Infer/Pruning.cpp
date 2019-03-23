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

#include "souper/Infer/AbstractInterpreter.h"
#include "souper/Infer/Pruning.h"

#include <cstdlib>

namespace souper {

std::string getUniqueName() {
  static int counter = 0;
  return "dummy" + std::to_string(counter++);
}

// TODO : Comment out debug stmts and conditions before benchmarking
bool PruningManager::isInfeasible(souper::Inst *RHS,
                                 unsigned StatsLevel) {
  for (int I = 0; I < InputVals.size(); ++I) {
    if (StatsLevel > 2) {
      llvm::errs() << "  Input:\n";
      for (auto &&p : InputVals[I]) {
        if (p.second.hasValue()) {
          llvm::errs() << "  Var " << p.first->Name << " : "
                        << p.second.getValue() << "\n";
        }
      }
    }

    if (LHSHasPhi) {
      auto LHSCR = LHSConstantRange[I];
      auto RHSCR = ConstantRangeAnalysis().findConstantRange(RHS, ConcreteInterpreters[I]);
      if (LHSCR.intersectWith(RHSCR).isEmptySet()) {
        if (StatsLevel > 2) {
          llvm::errs() << "  LHS ConstantRange = " << LHSCR << "\n";
          llvm::errs() << "  RHS ConstantRange = " << RHSCR << "\n";
          llvm::errs() << "  pruned phi-LHS using CR! ";
            if (!isConcrete(RHS, false, true)) {
              llvm::errs() << "Inst had a hole.";
            } else {
              llvm::errs() << "Inst had a symbolic const.";
            }
        }
        return true;
      }

      auto LHSKB = LHSKnownBits[I];
      auto RHSKB = KnownBitsAnalysis().findKnownBits(RHS, ConcreteInterpreters[I]);
      if ((LHSKB.Zero & RHSKB.One) != 0 || (LHSKB.One & RHSKB.Zero) != 0) {
        if (StatsLevel > 2) {
          llvm::errs() << "  LHS KnownBits = " << KnownBitsAnalysis::knownBitsString(LHSKB) << "\n";
          llvm::errs() << "  RHS KnownBits = " << KnownBitsAnalysis::knownBitsString(RHSKB) << "\n";
          llvm::errs() << "  pruned phi-LHS using KB! ";
            if (!isConcrete(RHS, false, true)) {
              llvm::errs() << "Inst had a hole.";
            } else {
              llvm::errs() << "Inst had a symbolic const.";
            }
        }
        return true;
      }

    } else {
      auto C = ConcreteInterpreters[I].evaluateInst(SC.LHS);
      if (C.hasValue()) {
        auto Val = C.getValue();
        if (StatsLevel > 2)
          llvm::errs() << "  LHS value = " << Val << "\n";
        if (!isConcrete(RHS)) {
          auto CR = ConstantRangeAnalysis().findConstantRange(RHS, ConcreteInterpreters[I]);
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
          auto KB = KnownBitsAnalysis().findKnownBits(RHS, ConcreteInterpreters[I]);
          if (StatsLevel > 2)
            llvm::errs() << "  RHS KnownBits = " << KnownBitsAnalysis::knownBitsString(KB) << "\n";
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
          auto RHSV = ConcreteInterpreters[I].evaluateInst(RHS);
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
  }

  if (!LHSHasPhi) {
    return isInfeasibleWithSolver(RHS, StatsLevel);
  } else {
    return false;
  }
}

bool PruningManager::isInfeasibleWithSolver(Inst *RHS, unsigned StatsLevel) {
  for (int I = 0; I < InputVals.size(); ++I) {
    auto C = ConcreteInterpreters[I].evaluateInst(SC.LHS);
    if (C.hasValue()) {
      auto Val = C.getValue();
      if (!isConcrete(RHS, false, true)) {
        std::vector<Inst *> Holes, ModelVars;
        getHoles(RHS, Holes);
        std::map<Inst *, Inst *> InstCache;
        std::vector<Inst *> Empty;
        for (auto *Hole : Holes) {
          auto DummyVar = SC.IC.createVar(Hole->Width, getUniqueName());
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
        auto RHSReplacement = getInstCopy(RHS, SC.IC, InstCache, BlockCache, &ConstMap, true);
        auto LHSReplacement = SC.IC.getConst(Val);

        auto Cond = SC.IC.getInst(Inst::Eq, 1, {LHSReplacement, RHSReplacement});
        InstMapping Mapping {Cond, SC.IC.getConst(llvm::APInt(1, true))};
        auto Query = BuildQuery(SC.IC, {}, {}, Mapping, &ModelVars, nullptr, true);
        if (StatsLevel > 3) {
          llvm::errs() << Query << "\n";

          llvm::errs() << "LHS\n";
          ReplacementContext RC1; RC1.printInst(Mapping.LHS, llvm::errs(), true);
          llvm::errs() << "RHS\n";
          ReplacementContext RC2; RC2.printInst(Mapping.RHS, llvm::errs(), true);
        }

        bool Result;
        std::vector<llvm::APInt> Models(ModelVars.size());
        auto EC = SC.SMTSolver->isSatisfiable(Query, Result, Models.size(), &Models, 1000);

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
  souper::SynthesisContext &SC_, std::vector<Inst*> &Inputs_, unsigned StatsLevel_)
                  : SC(SC_), NumPruned(0),
                    TotalGuesses(0),
                    StatsLevel(StatsLevel_),
                    InputVars(Inputs_) {}

void PruningManager::init() {

  Ante = SC.IC.getConst(llvm::APInt(1, true));
  for (auto PC : SC.PCs ) {
    Inst *Eq = SC.IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
    Ante = SC.IC.getInst(Inst::And, 1, {Ante, Eq});
  }

  findVars(Ante, InputVars);

  InputVals = generateInputSets(InputVars);

  for (auto &&Input : InputVals) {
    ConcreteInterpreters.emplace_back(SC.LHS, Input);
  }

  if (hasGivenInst(SC.LHS, [](Inst *I){ return I->K == Inst::Phi;})) {
    // Have to abstract interpret LHS because of phi
    LHSHasPhi = true;
    for (unsigned I = 0; I < InputVals.size(); I++) {
      LHSKnownBits.push_back(KnownBitsAnalysis().findKnownBits(SC.LHS, ConcreteInterpreters[I]));
      LHSConstantRange.push_back(ConstantRangeAnalysis().findConstantRange(SC.LHS, ConcreteInterpreters[I]));
    }
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

bool PruningManager::isInputValid(ValueCache &Cache) {
  ConcreteInterpreter CI(SC.LHS, Cache);

  if (auto V = CI.evaluateInst(Ante); V.hasValue() && V.getValue().getLimitedValue() == 1)
    return true;

  if (StatsLevel > 2) {
    llvm::errs() << "Input failed PC check: ";
    for (auto &&p : Cache) {
      if (p.first->K != Inst::Var)
        continue;

      if (p.second.hasValue()) {
        llvm::errs() << "  Var " << p.first->Name << " : "
                     << p.second.getValue() << ", ";
      }
    }
    llvm::errs() << '\n';
  }

  return false;
}

namespace {
  llvm::APInt getSpecialAPInt(char C, unsigned Width) {
    switch (C) {
    case 'a':
      return llvm::APInt(Width, -1);
    case 'b':
      return llvm::APInt(Width, 1);
    case 'c':
      return llvm::APInt(Width, 0);
    case 'd':
      return llvm::APInt::getSignedMaxValue(Width);
    case 'e':
      return llvm::APInt::getSignedMinValue(Width);
    }
    return llvm::APInt::getSignedMinValue(Width);
  }
} // anon

std::vector<ValueCache> PruningManager::generateInputSets(
  std::vector<Inst *> &Inputs) {
  std::vector<ValueCache> InputSets;

  ValueCache Cache;

  constexpr unsigned PermutedLimit = 15;
  std::string specialInputs = "abcde";
  std::unordered_set<std::string> Visited;
  do {
    int i = 0;
    std::string usedInput;
    for (auto &&I : Inputs) {
      if (I->K == souper::Inst::Var) {
	usedInput.append(1, specialInputs[i]);
	Cache[I] = getSpecialAPInt(specialInputs[i++], I->Width);
      }
    }

    if (!Visited.count(usedInput) && isInputValid(Cache)) {
      if (InputSets.size() >= PermutedLimit) break;
      InputSets.push_back(Cache);
      Visited.insert(usedInput);
    }
  } while (std::next_permutation(specialInputs.begin(), specialInputs.end()));


  for (auto &&I : Inputs) {
    if (I->K == souper::Inst::Var)
      Cache[I] = {llvm::APInt(I->Width, 0)};
  }
  if (isInputValid(Cache))
    InputSets.push_back(Cache);

  for (auto &&I : Inputs) {
    if (I->K == souper::Inst::Var)
      Cache[I] = {llvm::APInt(I->Width, 1)};
  }
  if (isInputValid(Cache))
    InputSets.push_back(Cache);

  for (auto &&I : Inputs) {
    if (I->K == souper::Inst::Var)
      Cache[I] = {llvm::APInt(I->Width, -1)};
  }
  if (isInputValid(Cache))
    InputSets.push_back(Cache);

  for (auto &&I : Inputs) {
    if (I->K == souper::Inst::Var)
      Cache[I] = {llvm::APInt::getSignedMaxValue(I->Width)};
  }
  if (isInputValid(Cache))
    InputSets.push_back(Cache);

  for (auto &&I : Inputs) {
    if (I->K == souper::Inst::Var)
      Cache[I] = {llvm::APInt::getSignedMinValue(I->Width)};
  }
  if (isInputValid(Cache))
    InputSets.push_back(Cache);

  constexpr int MaxTries = 100;
  constexpr int NumLargeInputs = 5;
  std::srand(0);
  int i, m;
  for (i = 0, m = 0; i < NumLargeInputs && m < MaxTries; ++m ) {
    for (auto &&I : Inputs) {
      if (I->K == souper::Inst::Var)
        Cache[I] = {llvm::APInt(I->Width, std::rand() % llvm::APInt(I->Width, -1).getLimitedValue())};
    }
    if (isInputValid(Cache)) {
      i++;
      InputSets.push_back(Cache);
    }
  }

  if (StatsLevel > 2 && i < NumLargeInputs) {
    llvm::errs() << "MaxTries (100) exhausted searching for large inputs.\n";
  }

  constexpr int NumSmallInputs = 5;
  for (i = 0, m = 0; i < NumSmallInputs && m < MaxTries; ++m ) {
    for (auto &&I : Inputs) {
      if (I->K == souper::Inst::Var)
        Cache[I] = {llvm::APInt(I->Width, std::rand() % I->Width)};
    }
    if (isInputValid(Cache)) {
      i++;
      InputSets.push_back(Cache);
    }
  }

  if (StatsLevel > 2 && i < NumSmallInputs) {
    llvm::errs() << "MaxTries (100) exhausted searching for small inputs.\n";
  }

  return InputSets;
}

}
