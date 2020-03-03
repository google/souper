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
#include "souper/Extractor/Candidates.h"
#include <cstdlib>

namespace souper {

std::string getUniqueName() {
  static int counter = 0;
  return "dummy" + std::to_string(counter++);
}

llvm::ConstantRange mkCR(llvm::APInt Low, llvm::APInt High) {
  return llvm::ConstantRange(Low, High);
}

llvm::ConstantRange mkCR(Inst *I, size_t Low, size_t High) {
  return llvm::ConstantRange(llvm::APInt(I->Width, Low),
                             llvm::APInt(I->Width, High));
}

bool isRangeInfeasible(Inst *C, llvm::APInt LHSV, Inst *RHS,
                       llvm::ConstantRange Range, ConcreteInterpreter &I) {
  std::unordered_map<Inst *, llvm::ConstantRange> CRCache;
  CRCache.insert({C, Range});
  return !ConstantRangeAnalysis(CRCache)
         .findConstantRange(RHS, I)
         .contains(LHSV);
}

#define MAX_PARTS 10
// Synthesized constant can only be in one of the ranges returned by this function
std::vector<llvm::ConstantRange> constantRangeNarrowing
  (Inst *C, llvm::APInt LHSV, Inst *RHS, ConcreteInterpreter &I,
   std::vector<llvm::ConstantRange> Stack,
   size_t MinRangeSize = 4, size_t MaxPartitions = MAX_PARTS) {

  if (Stack.size() >= MaxPartitions) {
    return Stack;
  }
  std::vector<llvm::ConstantRange> Result;
  auto testCR = [RHS, LHSV, C, &I] (llvm::ConstantRange CR) {
    return !isRangeInfeasible(C, LHSV, RHS, CR, I);
  };

  while (!Stack.empty()) {
    auto CR = Stack.back();
    Stack.pop_back();
    if (CR.isEmptySet()) {
      continue;
    }
    if (testCR(CR)) {
      // C could be in CR, subdivide
      auto L = CR.getLower();
      auto H = CR.getUpper();
      auto Size = getSetSize(CR);

      if (L.ugt(H)) {
        // TODO(manasij): Bisect wrapped ranges instead of giving up.
        Result.push_back(CR);
      } else if (Size.getLimitedValue() < MinRangeSize
                  || Result.size() + Stack.size() >= MaxPartitions) {
        Result.push_back(CR);
      } else {
        auto Mid = (L + H).udiv(llvm::APInt(C->Width, 2));
        auto Left = mkCR(L, Mid);
        auto Right = mkCR(Mid, H);
        if (testCR(Right)) {
          Stack.push_back(Right);
        }
        if (testCR(Left)) {
          Stack.push_back(Left);
        }
      }
    }
  }

  return Result;
}

std::pair<llvm::APInt, llvm::APInt> knownBitsNarrowing
  (Inst *C, llvm::APInt LHSV, Inst *RHS, ConcreteInterpreter &I,
   llvm::APInt KnownNotZero, llvm::APInt KnownNotOne) {

  for (int i = 0; i < C->Width; ++i) {
    llvm::KnownBits iZero(C->Width), iOne(C->Width);
    iZero.Zero |= 1 << i;
    iOne.One |= 1 << i;
    std::unordered_map<souper::Inst *, llvm::KnownBits>
      AssumeIZero{{C, iZero}},
      AssumeIOne{{C, iOne}};

    auto KB0 = KnownBitsAnalysis(AssumeIZero).findKnownBits(RHS, I);
    auto KB1 = KnownBitsAnalysis(AssumeIOne).findKnownBits(RHS, I);

    if ((KB0.Zero & LHSV) != 0 || (KB0.One & ~LHSV) != 0) {
      KnownNotZero |= 1 << i;
    }

    if ((KB1.Zero & LHSV) != 0 || (KB1.One & ~LHSV) != 0) {
      KnownNotOne |= 1 << i;
    }
  }

  return {KnownNotZero, KnownNotOne};
}

// TODO : Comment out debug stmts and conditions before benchmarking
bool PruningManager::isInfeasible(souper::Inst *RHS,
                                 unsigned StatsLevel) {
  std::unordered_map<Inst *, ExprInfo> RHSInfo = LHSInfo;
  ExprInfo::analyze(RHS, RHSInfo);
  bool HasHole = RHSInfo[RHS].HasHole;
  bool RHSIsConcrete = !RHSInfo[RHS].HasHole && !RHSInfo[RHS].HasConst;

  std::map<Inst *, std::vector<llvm::ConstantRange>> ConstantLimits;
  std::map<Inst *, llvm::APInt> ConstantKnownNotZero;
  std::map<Inst *, llvm::APInt> ConstantKnownNotOne;

  std::set<souper::Inst *> Constants;
  getConstants(RHS, Constants);

  if (HA.findIfHole(RHS)) {
    // Do not attempt pruning if the RHS will provably produce top
    // for all abstract interpreters
    return false;
  }

  if (!Constants.empty()) {
    auto RestrictedBits = RestrictedBitsAnalysis().findRestrictedBits(RHS);
    if ((~RestrictedBits & (LHSKnownBitsNoSpec.Zero | LHSKnownBitsNoSpec.One)) != 0) {
//     if (RestrictedBits == 0 && (LHSKB.Zero != 0 || LHSKB.One != 0)) {
      if (StatsLevel > 2) {
        llvm::errs() << "  pruned using restricted bits analysis.\n";
        llvm::errs() << "  LHSKB : " << KnownBitsAnalysis::knownBitsString(LHSKnownBitsNoSpec) << "\n";
        llvm::errs() << "  RB    : " << RestrictedBits.toString(2, false) << "\n";
      }
      return true;
    }

//     auto LHSCR = ConstantRangeAnalysis().findConstantRange(SC.LHS, BlankCI, false);
//     if (StatsLevel > 2) {
//         llvm::errs() << "  LHSCR : " << LHSCR << "\n";
//         llvm::errs() << "  RB    : " << RestrictedBits.toString(2, false) << "\n";
//     }
//     if (RestrictedBits == 0 && !LHSCR.isFullSet()) {
//       if (StatsLevel > 2) {
//         llvm::errs() << "  pruned using restricted bits cr analysis.\n";
//         llvm::errs() << "  LHSCR : " << LHSCR << "\n";
//         llvm::errs() << "  RB    : " << RestrictedBits.toString(2, false) << "\n";
//       }
//       return true;
//     }

    for (auto C : Constants) {
      auto CutOff = 0xFFFFFF;
      ConstantLimits[C].push_back(mkCR(C, 1, CutOff));
      ConstantLimits[C].push_back(mkCR(C, 0, CutOff).inverse());
      // ^ Initialize with full-set instead of this when we can gracefully deal with wrapped ranges

      ConstantKnownNotOne[C] = llvm::APInt(C->Width, 0);
      ConstantKnownNotZero[C] = llvm::APInt(C->Width, 0);
    }
  }

  if (!HasHole) {
    auto DontCareBits = DontCareBitsAnalysis().findDontCareBits(RHS);

    for (auto Pair : LHSMustDemandedBits) {
      if (DontCareBits.find(Pair.first) != DontCareBits.end() && (Pair.second & DontCareBits[Pair.first]) != 0) {
        // This input is must demanded in LHS and DontCare in RHS.
        if (StatsLevel > 2) {
          llvm::errs() << "Var : " << Pair.first->Name << " : ";
          llvm::errs() << Pair.second.toString(2, false) << "\t"
                       << DontCareBits[Pair.first].toString(2, false) << "\n";
          llvm::errs() << "  pruned using demanded bits analysis.\n";
        }

        return true;
      }
    }
  }

  bool FoundNonTopAnalysisResult = false;

  for (int I = 0; I < InputVals.size(); ++I) {
    if (I > 9 && !FoundNonTopAnalysisResult) {
      break;
      // Give up if first 10 known bits and constant range results
      // are all TOP.
      // TODO: Maybe figure out a better way to determine if an RHS
      // is not amenable to dataflow analysis. (example : x + HOLE)
      // TODO: Maybe tune this threshold, or make it user controllable
      // N = 10 results in a 2x performance boost for a 5% increase in
      // the final number of guesses.
      // Might make sense to make this threshold depend on the RHS
    }
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
      if (!RHSCR.isFullSet()) {
        FoundNonTopAnalysisResult = true;
      }
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
      if (!RHSKB.isUnknown()) {
        FoundNonTopAnalysisResult = true;
      }
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
        if (!RHSIsConcrete) {
          auto CR = ConstantRangeAnalysis().findConstantRange(RHS, ConcreteInterpreters[I]);
          if (StatsLevel > 2)
            llvm::errs() << "  RHS ConstantRange = " << CR << "\n";
          if (!CR.contains(Val)) {
            if (StatsLevel > 2) {
              llvm::errs() << "  pruned using CR! ";
              if (HasHole) {
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
              if (HasHole) {
                llvm::errs() << "Inst had a hole.";
              } else {
                llvm::errs() << "Inst had a symbolic const.";
              }
              llvm::errs() << "\n";
            }
            return true;
          }

          if (!HasHole) {
            // !Concrete and !HasHole, must have a Symbolic Constant
            for (auto C : Constants) {
              if (ConstantLimits[C].size() <= MAX_PARTS) {
                ConstantLimits[C] = constantRangeNarrowing(C, Val, RHS,
                                      ConcreteInterpreters[I],
                                      ConstantLimits[C]);
                if (!ConstantLimits[C].empty()) {
                  if (StatsLevel > 2) {
                    llvm::errs() << "  Constant narrowing possibility. Refine restriction to: ";
                    for (auto X : ConstantLimits[C]) {
                      llvm::errs() << X << "\t";
                    }
                    llvm::errs() << "\n";
                  }
                } else {
                  if (StatsLevel > 2) {
                    llvm::errs() << "  pruned using disjoint CR! ";
                      llvm::errs() << "Inst had a symbolic const.";
                    llvm::errs() << "\n";
                  }
                  return true;
                }
              }

              auto KBRefinement = knownBitsNarrowing(C, Val, RHS,
                                    ConcreteInterpreters[I],
                                    ConstantKnownNotZero[C],
                                    ConstantKnownNotOne[C]);
              ConstantKnownNotZero[C] = KBRefinement.first;
              ConstantKnownNotOne[C] = KBRefinement.second;
              llvm::KnownBits KNOTB;
              KNOTB.Zero = ConstantKnownNotZero[C];
              KNOTB.One = ConstantKnownNotOne[C];
              if (KNOTB.hasConflict()) {
                if (StatsLevel > 2) {
                  llvm::errs() << KNOTB.Zero.toString(2, false) << "\n" << KNOTB.One.toString(2, false) << "\n";
                  llvm::errs() << "  pruned using KB refinement! ";
                  llvm::errs() << "Inst had a symbolic const.";
                  llvm::errs() << "\n";
                }
                return true;
              } else {
                if (StatsLevel > 2) {
                  llvm::errs() << "  KNOTB refined to: " <<
                    Inst::getKnownBitsString(ConstantKnownNotZero[C],
                                              ConstantKnownNotOne[C]) << "\n";
                  if (KNOTB.isConstant()) {
                    auto Const = ~KNOTB.getConstant();
                    std::map<Inst *, llvm::APInt> CMap = {{C, Const}};

                    std::map<Inst *, Inst *> InstCache;
                    std::map<Block *, Block *> BlockCache;
                    Inst *RHSCopy = getInstCopy(RHS, SC.IC, InstCache, BlockCache, &CMap, false);

                    if (isInfeasible(RHSCopy, StatsLevel)) {
                      if (StatsLevel > 2) {
                        llvm::errs() << "  pruned using KNOTB instantiation!  ";
                        llvm::errs() << "Inst had a symbolic const.\n";
                      }
                      return true;
                    }
                  }
                }
              }

            }
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

  for (auto &C : ConstantLimits) {
    auto &Rs = C.second;
    if (!Rs.empty()) {
      // The other case is handled in the input specialization loop
      if (StatsLevel > 2) {
        llvm::errs() << "  Constant narrowing possibility. Restrict to: ";
        for (auto X : Rs) {
          llvm::errs() << X << "\t";
        }
        llvm::errs() << "\n";
      }

      size_t ResidualSize = 0;
      for (auto &&R : Rs) {
        ResidualSize += getSetSize(R).getLimitedValue();
      }

      if (ResidualSize < 8192 && Rs.size() < 3) {
        // TODO: Tune. These thresholds control when the solver is involved
        C.first->RangeRefinement = Rs;
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
      RC.printInst(SC.LHS, llvm::errs(), true);
      llvm::errs() << "=>?\n";
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

  ConcreteInterpreter BlankCI;
  LHSKnownBitsNoSpec =  KnownBitsAnalysis().findKnownBits(SC.LHS, BlankCI, false);
  LHSMustDemandedBits = MustDemandedBitsAnalysis().findMustDemandedBits(SC.LHS);

  ExprInfo::analyze(SC.LHS, LHSInfo);
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

void ExprInfo::analyze(Inst *Root,
                       std::unordered_map<Inst *, ExprInfo> &Result) {
  ExprInfo EI{false, false, false};
  if (Root->K == Inst::ReservedConst ||
     (Root->K == Inst::Var && Root->SynthesisConstID != 0)) {
    EI.HasConst = true;
  } else if (Root->K == Inst::Var) {
    EI.HasInput = true;
  }
  if (Root->K == Inst::Hole) {
    EI.HasHole = true;
  }
  for (auto &&Op : Root->Ops) {
    analyze(Op, Result);
    auto &Part = Result[Op];
    EI.HasHole |= Part.HasHole;
    EI.HasConst |= Part.HasConst;
    EI.HasInput |= Part.HasInput;
  }
  Result[Root] = EI;
}
}
