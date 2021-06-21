// Copyright 2014 The Souper Authors. All rights reserved.
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

#define DEBUG_TYPE "souper"

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Instruction.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/KnownBits.h"
#include "souper/Codegen/Codegen.h"
#include "souper/Extractor/Solver.h"
#include "souper/Infer/AliveDriver.h"
#include "souper/Infer/Z3Driver.h"
#include "souper/Infer/ConstantSynthesis.h"
#include "souper/Infer/EnumerativeSynthesis.h"
#include "souper/Infer/InstSynthesis.h"
#include "souper/Infer/Preconditions.h"
#include "souper/Infer/Pruning.h"
#include "souper/KVStore/KVStore.h"
#include "souper/Parser/Parser.h"

#include <unordered_map>

STATISTIC(MemHitsInfer, "Number of internal cache hits for infer()");
STATISTIC(MemMissesInfer, "Number of internal cache misses for infer()");
STATISTIC(MemHitsIsValid, "Number of internal cache hits for isValid()");
STATISTIC(MemMissesIsValid, "Number of internal cache misses for isValid()");
STATISTIC(ExternalHits, "Number of external cache hits");
STATISTIC(ExternalMisses, "Number of external cache misses");

using namespace souper;
using namespace llvm;

namespace {

static cl::opt<bool> NoInfer("souper-no-infer",
    cl::desc("Populate the external cache, but don't infer replacements (default=false)"),
    cl::init(false));
static cl::opt<bool> UseZ3Driver("souper-in-process-z3",
    cl::desc("Use Z3 C++ api instead of smtlib2 (default=false)"),
    cl::init(false));

static cl::opt<bool> UseCegis("souper-use-cegis",
    cl::desc("Infer instructions (default=false)"),
    cl::init(false));
static cl::opt<int> MaxLHSSize("souper-max-lhs-size",
    cl::desc("Max size of LHS (in bytes) to put in external cache (default=1024)"),
    cl::init(1024));
static cl::opt<int> MaxConstantSynthesisTries("souper-max-constant-synthesis-tries",
    cl::desc("Max number of constant synthesis tries. (default=30)"),
    cl::init(30));


class BaseSolver : public Solver {
  std::unique_ptr<SMTLIBSolver> SMTSolver;
  unsigned Timeout;

public:
  BaseSolver(std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout)
      : SMTSolver(std::move(SMTSolver)), Timeout(Timeout) {}

  void findVarsAndWidth(Inst *Node, std::map<std::string, unsigned> &VarsVect,
                        std::set<Inst *> &Visited) {
    if (!Visited.insert(Node).second)
      return;
    if (Node->K == Inst::Var) {
      std::string Name = Node->Name;
      VarsVect.insert(std::pair<std::string, unsigned>(Name, Node->Width));
    }
    for (auto const &Op : Node->Ops) {
      findVarsAndWidth(Op, VarsVect, Visited);
    }
  }

  void findMoreVarsViaPC(Inst *Node,
                         std::map<std::string, unsigned> &VarsVect,
                         std::set<Inst *> &Visited) {
    if (!Visited.insert(Node).second)
      return;
    if (Node->K == Inst::Var) {
      std::string Name = Node->Name;
      VarsVect.insert(std::pair<std::string, unsigned>(Name, Node->Width));
    }
    for (auto const &Op : Node->Ops) {
      findMoreVarsViaPC(Op, VarsVect, Visited);
    }
  }

  llvm::APInt getClearedBit(unsigned Pos, unsigned W) {
    APInt AllOnes = APInt::getAllOnesValue(W);
    AllOnes.clearBit(Pos);
    return AllOnes;
  }

  Inst *traverse(Inst *Node, unsigned BitPos, InstContext &IC,
                 std::string VarName,
                 std::map<Inst *, Inst *> &InstCache, bool SetBit) {
    if (InstCache.count(Node))
      return InstCache.at(Node);
    std::vector<Inst *> Ops;
    for (auto const &Op : Node->Ops) {
      if (SetBit)
        Ops.push_back(traverse(Op, BitPos, IC, VarName, InstCache, true));
      else
        Ops.push_back(traverse(Op, BitPos, IC, VarName, InstCache, false));
    }

    Inst *Copy = nullptr;
    if (Node->K == Inst::Var && Node->Name == VarName) {
      unsigned VarWidth = Node->Width;
      if (SetBit) {
        APInt SetBit = APInt::getOneBitSet(VarWidth, BitPos);
        Inst *SetMask = IC.getInst(Inst::Or, VarWidth,
                                   {Node, IC.getConst(SetBit)});
        Copy = SetMask;
      } else {
        APInt ClearBit = getClearedBit(BitPos, VarWidth);
        Inst *ClearMask = IC.getInst(Inst::And, VarWidth,
                                     {Node, IC.getConst(ClearBit)});
        Copy = ClearMask;
      }
    } else if (Node->K == Inst::Var && Node->Name != VarName) {
      Copy = Node;
    } else if (Node->K == Inst::Const || Node->K == Inst::UntypedConst) {
      Copy = Node;
    } else if (Node->K == Inst::Phi) {
      Copy = IC.getPhi(Node->B, Ops);
    } else {
      Copy = IC.getInst(Node->K, Node->Width, Ops);
    }
    assert(Copy);
    InstCache[Node] = Copy;
    return Copy;
  }

  bool testDB(const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
              Inst *LHS, Inst *NewLHS, InstContext &IC) {
    unsigned W = LHS->Width;
    Inst *Ne = IC.getInst(Inst::Ne, 1, {LHS, NewLHS});
    Inst *Ante = IC.getConst(APInt(1, 1));
    Ante = IC.getInst(Inst::And, 1, {Ante, Ne});
    APInt TrueGuess(1, 1, false);
    Inst *True = IC.getConst(TrueGuess);
    InstMapping Mapping(Ante, True);

    bool IsSat;
    std::string Query = BuildQuery(IC, BPCs, PCs, Mapping, 0,
                                   /*Precondition=*/0, true);
    std::error_code EC = SMTSolver->isSatisfiable(Query, IsSat, 0, 0, Timeout);

    if (EC)
      llvm::report_fatal_error("stopping due to error");
    return !IsSat;
  }

  std::error_code testDemandedBits(const BlockPCs &BPCs,
                                   const std::vector<InstMapping> &PCs,
                                   Inst *LHS,
                                   std::map<std::string, APInt> &ResDBVect,
                                   InstContext &IC) override {
    unsigned W = LHS->Width;

    if (!LHS->DemandedBits.isAllOnesValue()) {
      LHS = IC.getInst(Inst::And, W, {LHS, IC.getConst(LHS->DemandedBits)});
    }

    std::map<Inst *, Inst *> InstCache;
    std::map<Block *, Block *> BlockCache;

    std::map<std::string, unsigned> VarsVect;
    std::set<Inst *> Visited;
    findVarsAndWidth(LHS, VarsVect, Visited);

    for (auto const &PC : PCs) {
      Visited.clear();
      findMoreVarsViaPC(PC.LHS, VarsVect, Visited);
      Visited.clear();
      findMoreVarsViaPC(PC.RHS, VarsVect, Visited);
    }

    for (std::map<std::string,unsigned>::iterator it = VarsVect.begin();
         it != VarsVect.end(); ++it) {
       std::string VarName = it->first;
       unsigned VarWidth = VarsVect[VarName];
       APInt ResultDB = APInt::getNullValue(VarWidth);

      for (unsigned Bit=0; Bit<VarWidth; Bit++) {
        std::map<Inst *, Inst *> InstCache;
        Inst *SetLHS = traverse(LHS, Bit, IC, VarName, InstCache, true);
        InstCache.clear();
        Inst *ClearLHS = traverse(LHS, Bit, IC, VarName, InstCache, false);
        if (testDB(BPCs, PCs, LHS, SetLHS, IC) &&
            testDB(BPCs, PCs, LHS, ClearLHS, IC)) {
          ResultDB = ResultDB;
        } else {
          ResultDB |= APInt::getOneBitSet(VarWidth, Bit);
        }
      }
      ResDBVect[VarName] = ResultDB;
    }
    return std::error_code();
  }

  bool testZeroMSB(const BlockPCs &BPCs,
                   const std::vector<InstMapping> &PCs,
                   Inst *LHS, InstContext &IC) {
    unsigned W = LHS->Width;
    Inst *Mask = IC.getConst(APInt::getOneBitSet(W, W-1));
    InstMapping Mapping(IC.getInst(Inst::And, W, { LHS, Mask }), IC.getConst(APInt::getNullValue(W)));
    bool IsSat;
    std::error_code EC = SMTSolver->isSatisfiable(BuildQuery(IC, BPCs, PCs,
                                                  Mapping, 0, /*Precondition=*/0),
                                                  IsSat, 0, 0, Timeout);
    if (EC) {
      llvm::report_fatal_error("Error: SMTSolver->isSatisfiable() failed in testing zero MSB");
      return false;
    }
    return !IsSat;
  }

  bool testOneMSB(const BlockPCs &BPCs,
                  const std::vector<InstMapping> &PCs,
                  Inst *LHS, InstContext &IC) {
    unsigned W = LHS->Width;
    Inst *Mask = IC.getConst(APInt::getOneBitSet(W, W-1));
    InstMapping Mapping(IC.getInst(Inst::And, W, { LHS, Mask }), Mask);
    bool IsSat;
    std::error_code EC = SMTSolver->isSatisfiable(BuildQuery(IC, BPCs, PCs,
                                                  Mapping, 0, /*Precondition=*/0),
                                                  IsSat, 0, 0, Timeout);
    if (EC) {
      llvm::report_fatal_error("Error: SMTSolver->isSatisfiable() failed in testing one MSB");
      return false;
    }
    return !IsSat;
  }

  std::error_code negative(const BlockPCs &BPCs,
                           const std::vector<InstMapping> &PCs,
                           Inst *LHS, bool &Negative,
                           InstContext &IC) override {
    Negative = false;
    if (testOneMSB(BPCs, PCs, LHS, IC))
      Negative = true;
    return std::error_code();
  }

  std::error_code nonNegative(const BlockPCs &BPCs,
                              const std::vector<InstMapping> &PCs,
                              Inst *LHS, bool &NonNegative,
                              InstContext &IC) override {
    NonNegative = false;
    if (testZeroMSB(BPCs, PCs, LHS, IC))
      NonNegative = true;
    return std::error_code();
  }

  std::error_code abstractPrecondition(const BlockPCs &BPCs,
                  const std::vector<InstMapping> &PCs,
                  InstMapping &Mapping, InstContext &IC,
                  bool &FoundWeakest) override {
    SynthesisContext SC{IC, SMTSolver.get(), Mapping.LHS, /*LHSUB*/nullptr, PCs,
                      BPCs, /*CheckAllGuesses=*/false, Timeout};

    std::vector<std::map<Inst *, llvm::KnownBits>> Results =
            inferAbstractKBPreconditions(SC, Mapping.RHS, SMTSolver.get(), this, FoundWeakest);

    ReplacementContext RC;
    auto LHSStr = RC.printInst(Mapping.LHS, llvm::outs(), true);
    llvm::outs() << "infer " << LHSStr << "\n";
    auto RHSStr = RC.printInst(Mapping.RHS, llvm::outs(), true);
    llvm::outs() << "result " << RHSStr << "\n";
    for (size_t i = 0; i < Results.size(); ++i) {
      for (auto It = Results[i].begin(); It != Results[i].end(); ++It) {
        auto &&P = *It;
        std::string dummy;
        llvm::raw_string_ostream str(dummy);
        auto VarStr = RC.printInst(P.first, str, false);
        llvm::outs() << VarStr << " -> " << Inst::getKnownBitsString(P.second.Zero, P.second.One);

        auto Next = It;
        Next++;
        if (Next != Results[i].end()) {
          llvm::outs()  << " (and) ";
        }
      }
      if (i == Results.size() - 1) {
        llvm::outs() << "\n";
      } else  {
        llvm::outs() << "\n(or)\n";
      }
    }
    return {};
  }

  std::error_code knownBits(const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          Inst *LHS, KnownBits &Known,
                          InstContext &IC) override {
    unsigned W = LHS->Width;
    Known.One = APInt::getNullValue(W);
    Known.Zero = APInt::getNullValue(W);
    for (unsigned I=0; I<W; I++) {
      APInt ZeroGuess = Known.Zero | APInt::getOneBitSet(W, I);
      if (testKnown(BPCs, PCs, ZeroGuess, Known.One, LHS, IC)) {
        Known.Zero = ZeroGuess;
        continue;
      }
      APInt OneGuess = Known.One | APInt::getOneBitSet(W, I);
      if (testKnown(BPCs, PCs, Known.Zero, OneGuess, LHS, IC))
        Known.One = OneGuess;
    }
    return std::error_code();
  }

  std::error_code powerTwo(const BlockPCs &BPCs,
                           const std::vector<InstMapping> &PCs,
                           Inst *LHS, bool &PowTwo,
                           InstContext &IC) override {
    unsigned W = LHS->Width;
    Inst *PowerMask = IC.getInst(Inst::And, W,
                                 {IC.getInst(Inst::Sub, W,
                                             {LHS, IC.getConst(APInt(W, 1, false))}),
                                             LHS});
    Inst *Zero = IC.getConst(APInt(W, 0, false));
    Inst *True = IC.getConst(APInt(1, 1, false));
    Inst *PowerTwoInst = IC.getInst(Inst::And, 1, {IC.getInst(Inst::Ne, 1, {LHS, Zero}),
                                    IC.getInst(Inst::Eq, 1, {PowerMask, Zero})});
    InstMapping Mapping(PowerTwoInst, True);
    bool IsSat;
    std::error_code EC = SMTSolver->isSatisfiable(BuildQuery(IC, BPCs, PCs,
                                                  Mapping, 0, /*Precondition=*/0),
                                                  IsSat, 0, 0, Timeout);
    if (EC)
      llvm::report_fatal_error("Error: SMTSolver->isSatisfiable() failed in testing powerTwo");

    if (!IsSat)
      PowTwo = true;
    else
      PowTwo = false;
    return std::error_code();
  }

  std::error_code nonZero(const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          Inst *LHS, bool &NonZero,
                          InstContext &IC) override {
    unsigned W = LHS->Width;
    Inst *Zero = IC.getConst(APInt(W, 0, false));
    Inst *True = IC.getConst(APInt(1, 1, false));
    Inst *NonZeroGuess = IC.getInst(Inst::Ne, 1, {LHS, Zero});
    InstMapping Mapping(NonZeroGuess, True);
    bool IsSat;
    std::error_code EC = SMTSolver->isSatisfiable(BuildQuery(IC, BPCs, PCs,
                                                  Mapping, 0, /*Precondition=*/0),
                                                  IsSat, 0, 0, Timeout);
    if (EC)
      llvm::report_fatal_error("Error: SMTSolver->isSatisfiable() failed in testing nonZero");

    if (!IsSat)
      NonZero = true;
    else
      NonZero = false;
    return std::error_code();
  }

  std::error_code signBits(const BlockPCs &BPCs,
                           const std::vector<InstMapping> &PCs,
                           Inst *LHS, unsigned &SignBits,
                           InstContext &IC) override {
    unsigned W = LHS->Width;
    SignBits = 1;
    Inst *True = IC.getConst(APInt(1, 1, false));

    for (unsigned I=2; I<=W; I++) {
      Inst *ShiftAmt = IC.getConst(APInt(W, W-I, false));
      Inst *Res = IC.getInst(Inst::AShr, W, {LHS, ShiftAmt});
      Inst *Guess1 = IC.getInst(Inst::Eq, 1, {Res, IC.getConst(APInt(W, 0, false))});
      Inst *Guess2 = IC.getInst(Inst::Eq, 1, {Res, IC.getConst(APInt::getAllOnesValue(W))});
      Inst *Guess = IC.getInst(Inst::Or, 1, {Guess1, Guess2});
      InstMapping Mapping(Guess, True);
      bool IsSat;
      std::error_code EC = SMTSolver->isSatisfiable(BuildQuery(IC, BPCs, PCs,
                                                    Mapping, 0, /*Precondition=*/0),
                                                    IsSat, 0, 0, Timeout);
      if (EC)
        llvm::report_fatal_error("Error: SMTSolver->isSatisfiable() failed in testing sign bits");

      if (!IsSat) {
        SignBits = I;
      } else {
        break;
      }
    }
    return std::error_code();
  }

  std::error_code inferHelper(const BlockPCs &BPCs,
                              const std::vector<InstMapping> &PCs,
                              Inst *LHS, std::vector<Inst *> &RHSs,
                              bool AllowMultipleRHSs, InstContext &IC) {
    std::error_code EC;

    // FIXME -- it's a bit messy to have this custom logic here
    if (LHS->HarvestKind == HarvestType::HarvestedFromUse) {
      Inst *C = IC.createSynthesisConstant(LHS->Width, /*SynthesisConstID=*/1);
      if (UseAlive) {
        Inst *Ante = IC.getConst(llvm::APInt(1, true));
        for (auto PC : PCs ) {
          Inst *Eq = IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
          Ante = IC.getInst(Inst::And, 1, {Ante, Eq});
        }

        AliveDriver Synthesizer(LHS, Ante, IC);
        auto ConstantMap = Synthesizer.synthesizeConstantsWithCegis(C, IC);
        if (ConstantMap.find(C) != ConstantMap.end()) {
          RHSs.emplace_back(IC.getConst(ConstantMap[C]));
          return std::error_code();
        }
        // TODO: Propagate errors from Alive backend, exit early for errors
      } else {
        std::map<Inst *, llvm::APInt> ResultMap;
        std::set<Inst*> ConstSet{C};
        ConstantSynthesis CS;
        EC = CS.synthesize(SMTSolver.get(), BPCs, PCs, InstMapping(LHS, C), ConstSet,
                           ResultMap, IC, /*MaxTries=*/1, Timeout, /*AvoidNops=*/false);
        if (ResultMap.find(C) != ResultMap.end()) {
          RHSs.emplace_back(IC.getConst(ResultMap[C]));
          return std::error_code();
        }
      }
      // only synthesize a constant for values harvested from uses--
      // do not continue with more synthesis
      return EC;
    }

    if (UseCegis) {
      InstSynthesis IS;
      Inst *RHS;
      EC = IS.synthesize(SMTSolver.get(), BPCs, PCs, LHS, RHS, IC, Timeout);
      RHSs.emplace_back(RHS);
      if (EC || RHS)
        return EC;
    } else {
      EnumerativeSynthesis ES;
      EC = ES.synthesize(SMTSolver.get(), BPCs, PCs, LHS, RHSs,
                         AllowMultipleRHSs, IC, Timeout);
      if (EC || !RHSs.empty())
        return EC;
    }

    RHSs.clear();
    return EC;
  }

  std::error_code infer(const BlockPCs &BPCs,
                        const std::vector<InstMapping> &PCs,
                        Inst *LHS, std::vector<Inst *> &RHSs,
                        bool AllowMultipleRHSs, InstContext &IC) override {
    auto EC = inferHelper(BPCs, PCs, LHS, RHSs, AllowMultipleRHSs, IC);
    if (RHSs.size() <= 1)
      return EC;

    for (auto &RHS : RHSs) {
      BackendCost BC;
      getBackendCost(IC, RHS, BC);
      // FIXME sort the list
    }

    return EC;
  }

  std::error_code isValid(InstContext &IC, const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model)
  override {
    if (UseAlive) {
      IsValid = isTransformationValid(Mapping.LHS, Mapping.RHS, PCs, BPCs, IC);
      return std::error_code();
    } else if (UseZ3Driver) {
      IsValid = isTransformationValidZ3(Mapping.LHS, Mapping.RHS, PCs, BPCs, IC, Timeout);
      return std::error_code();
    }
    std::string Query;
    if (Model) {
      std::vector<Inst *> ModelInsts;
      std::string Query = BuildQuery(IC, BPCs, PCs, Mapping, &ModelInsts, /*Precondition=*/0);
      if (Query.empty())
        return std::make_error_code(std::errc::value_too_large);
      bool IsSat;
      std::vector<llvm::APInt> ModelVals;
      std::error_code EC = SMTSolver->isSatisfiable(
          Query, IsSat, ModelInsts.size(), &ModelVals, Timeout);
      if (!EC) {
        if (IsSat) {
          for (unsigned I = 0; I != ModelInsts.size(); ++I) {
            Model->push_back(std::make_pair(ModelInsts[I], ModelVals[I]));
          }
        }
        IsValid = !IsSat;
      }
      return EC;
    } else {
      std::string Query = BuildQuery(IC, BPCs, PCs, Mapping, 0, /*Precondition=*/0);
      if (Query.empty())
        return std::make_error_code(std::errc::value_too_large);
      bool IsSat;
      std::error_code EC = SMTSolver->isSatisfiable(Query, IsSat, 0, 0, Timeout);
      IsValid = !IsSat;
      return EC;
    }
  }

  std::error_code inferConst(const BlockPCs &BPCs,
                             const std::vector<InstMapping> &PCs,
                             Inst *LHS, Inst *&RHS,
                             std::set<Inst *> &ConstSet,
                             std::map<Inst *, llvm::APInt> &ResultMap,
                             InstContext &IC) override {
    SynthesisContext SC{IC, SMTSolver.get(), LHS, /*LHSUB*/nullptr, PCs,
                        BPCs, /*CheckAllGuesses=*/false, Timeout};
    // TODO: Construct LHSUB, a predicate which evaluates to true when corresponding inputs
    // case LHS to evaluate to UB
    std::vector<Inst *> Inputs;
    findVars(LHS, Inputs);
    PruningManager Pruner(SC, Inputs, DebugLevel);
    Pruner.init();
    ConstantSynthesis CS{&Pruner};
    std::error_code EC = CS.synthesize(SMTSolver.get(), BPCs, PCs, InstMapping(LHS, RHS),
                                       ConstSet, ResultMap, IC, MaxConstantSynthesisTries,
                                       Timeout, /*AvoidNops=*/false);

    if (EC || ResultMap.empty())
      return EC;

    std::map<Inst *, Inst *> InstCache;
    std::map<Block *, Block *> BlockCache;
    RHS = getInstCopy(RHS, IC, InstCache, BlockCache, &ResultMap, false);
    return EC;
  }

  bool testKnown(const BlockPCs &BPCs,
                 const std::vector<InstMapping> &PCs,
                 APInt &Zeros, APInt &Ones, Inst *LHS,
                 InstContext &IC) {
    InstMapping Mapping(IC.getInst(Inst::And, LHS->Width,
                                   { IC.getConst(Zeros | Ones), LHS }),
                        IC.getConst(Ones));
    bool IsSat;
    auto Q = BuildQuery(IC, BPCs, PCs, Mapping, 0, /*Precondition=*/0);
    std::error_code EC = SMTSolver->isSatisfiable(Q, IsSat, 0, 0, Timeout);
    if (EC) {
      llvm::report_fatal_error("Error: SMTSolver->isSatisfiable() failed in testing known bits");
      return false;
    }
    return !IsSat;
  }

  void testRange(const BlockPCs &BPCs,
                 const std::vector<InstMapping> &PCs,
                 Inst *LHS, llvm::APInt &C,
                 llvm::APInt &ResultX,
                 bool &IsFound,
                 InstContext &IC) {
    unsigned W = LHS->Width;

    Inst *ReservedX = IC.createSynthesisConstant(W, 1);
    Inst *CVal = IC.getConst(C);
    Inst *LowerVal = ReservedX;
    Inst *UpperValOverflow = IC.getInst(Inst::UAddWithOverflow, W + 1,
                                        {IC.getInst(Inst::Add, W, {LowerVal, CVal}),
                                         IC.getInst(Inst::UAddO, 1, {LowerVal, CVal})});

    Inst *IsOverflow = IC.getInst(Inst::ExtractValue, 1, {UpperValOverflow, IC.getUntypedConst(llvm::APInt(W, 1))});
    Inst *UpperVal = IC.getInst(Inst::ExtractValue, W, {UpperValOverflow, IC.getUntypedConst(llvm::APInt(W, 0))});

    Inst *GuessLowerPartNonWrapped = IC.getInst(Inst::Ule, 1, {LowerVal, LHS});
    Inst *GuessUpperPartNonWrapped = IC.getInst(Inst::Ult, 1, {LHS, UpperVal});

    // non-wrapped, x <= LHS < x+c
    Inst *GuessAnd = IC.getInst(Inst::And, 1, { GuessLowerPartNonWrapped, GuessUpperPartNonWrapped });
    // wrapped, LHS < x+c \/ LHS >= x
    Inst *GuessOr = IC.getInst(Inst::Or, 1, { GuessLowerPartNonWrapped, GuessUpperPartNonWrapped });

    // if x+c overflows, treat it as wrapped.
    Inst *Guess = IC.getInst(Inst::Select, 1, {IsOverflow, GuessOr, GuessAnd});

    std::set<Inst *> ConstSet{ReservedX};
    std::map <Inst *, llvm::APInt> ResultMap;
    ConstantSynthesis CS;
    // Before switching to ConcreteInterpreter for LHS simplification, the query is Guess(ReservedX, LHS) == 1
    // Note there is a reservedconst (ReservedX) in left side of the query. After the switch, the left side
    // of the query needs to be reservedconst free, and we still need LHS to stay on the left side of
    // the query to take care of UB, therefore, the new query is or(trunc(LHS), 1) = Guess(ReservedX, LHS)
    LHS = IC.getInst(Inst::Or, 1, {IC.getInst(Inst::Trunc, 1, {LHS}), IC.getConst(llvm::APInt(1, true))}),
    CS.synthesize(SMTSolver.get(), BPCs, PCs, InstMapping(LHS, Guess),
                  ConstSet, ResultMap, IC, MaxConstantSynthesisTries, Timeout,
                  /*AvoidNops=*/false);
    if (ResultMap.empty()) {
      IsFound = false;
    } else {
      IsFound = true;
      ResultX = ResultMap[ReservedX];
    }
  }

  llvm::ConstantRange constantRange(const BlockPCs &BPCs,
                                    const std::vector<InstMapping> &PCs,
                                    Inst *LHS,
                                    InstContext &IC) override {
    unsigned W = LHS->Width;

    APInt L = APInt(W, 1), R = APInt::getAllOnesValue(W);
    APInt BinSearchResultX, BinSearchResultC;
    bool BinSearchHasResult = false;

    while (L.ule(R)) {
      APInt M = L + ((R - L)).lshr(1);
      APInt BinSearchX;
      bool Found = false;
      testRange(BPCs, PCs, LHS, M, BinSearchX, Found, IC);
      if (Found) {
        R = M - 1;

        // record result
        BinSearchResultX = BinSearchX;
        BinSearchResultC = M;
        BinSearchHasResult = true;
      } else {
        if (L == R)
          break;
        L = M + 1;
      }
    }

    if (BinSearchHasResult) {
      return llvm::ConstantRange(BinSearchResultX, BinSearchResultX + BinSearchResultC);
    } else {
      return llvm::ConstantRange (W, true);
    }
  }

  std::string getName() override {
    return SMTSolver->getName();
  }
};

class MemCachingSolver : public Solver {
  std::unique_ptr<Solver> UnderlyingSolver;
  std::unordered_map<std::string, std::pair<std::error_code, bool>> IsValidCache;
  std::unordered_map<std::string, std::pair<std::error_code, std::string>>
    InferCache;

public:
  MemCachingSolver(std::unique_ptr<Solver> UnderlyingSolver)
      : UnderlyingSolver(std::move(UnderlyingSolver)) {}

  std::error_code infer(const BlockPCs &BPCs,
                        const std::vector<InstMapping> &PCs,
                        Inst *LHS, std::vector<Inst *> &RHSs,
                        bool AllowMultipleRHSs, InstContext &IC) override {
    ReplacementContext Context;
    std::string Repl = GetReplacementLHSString(BPCs, PCs, LHS, Context);
    const auto &ent = InferCache.find(Repl);
    if (ent == InferCache.end()) {
      ++MemMissesInfer;
      std::error_code EC = UnderlyingSolver->infer(BPCs, PCs, LHS, RHSs,
                                                   AllowMultipleRHSs, IC);
      std::string RHSStr;
      if (!EC && !RHSs.empty()) {
        // TODO: support multi RHSs caching
        RHSStr = GetReplacementRHSString(RHSs.front(), Context);
      }
      InferCache.emplace(Repl, std::make_pair(EC, RHSStr));
      return EC;
    } else {
      ++MemHitsInfer;
      std::string ES;
      StringRef S = ent->second.second;
      if (S == "") {
        RHSs.clear();
      } else {
        ParsedReplacement R = ParseReplacementRHS(IC, "<cache>", S, Context, ES);
        if (ES != "")
          return std::make_error_code(std::errc::protocol_error);
        RHSs.emplace_back(R.Mapping.RHS);
      }
      return ent->second.first;
    }
  }
  std::error_code inferConst(const BlockPCs &BPCs,
                             const std::vector<InstMapping> &PCs,
                             Inst *LHS, Inst *&RHS,
                             std::set<Inst *> &ConstSet,
                             std::map<Inst *, llvm::APInt> &ResultMap,
                             InstContext &IC) override {
    return UnderlyingSolver->inferConst(BPCs, PCs, LHS, RHS, ConstSet, ResultMap, IC);
  }

  llvm::ConstantRange constantRange(const BlockPCs &BPCs,
                                    const std::vector<InstMapping> &PCs,
                                    Inst *LHS,
                                    InstContext &IC) override {
    return UnderlyingSolver->constantRange(BPCs, PCs, LHS, IC);
  }

  std::error_code isValid(InstContext &IC, const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model)
    override {
    // TODO: add caching support for models.
    if (Model)
      return UnderlyingSolver->isValid(IC, BPCs, PCs, Mapping, IsValid, Model);

    std::string Repl = GetReplacementString(BPCs, PCs, Mapping);
    const auto &ent = IsValidCache.find(Repl);
    if (ent == IsValidCache.end()) {
      ++MemMissesIsValid;
      std::error_code EC = UnderlyingSolver->isValid(IC, BPCs, PCs,
                                                     Mapping, IsValid, 0);
      IsValidCache.emplace(Repl, std::make_pair(EC, IsValid));
      return EC;
    } else {
      ++MemHitsIsValid;
      IsValid = ent->second.second;
      return ent->second.first;
    }
  }

  std::string getName() override {
    return UnderlyingSolver->getName() + " + internal cache";
  }

  std::error_code testDemandedBits(const BlockPCs &BPCs,
                                   const std::vector<InstMapping> &PCs,
                                   Inst *LHS,
                                   std::map<std::string,APInt> &DBitsVect,
                                   InstContext &IC) override {
    return UnderlyingSolver->testDemandedBits(BPCs, PCs, LHS, DBitsVect, IC);
  }

  std::error_code nonNegative(const BlockPCs &BPCs,
                              const std::vector<InstMapping> &PCs,
                              Inst *LHS, bool &NonNegative,
                              InstContext &IC) override {
    return UnderlyingSolver->nonNegative(BPCs, PCs, LHS, NonNegative, IC);
  }

  std::error_code negative(const BlockPCs &BPCs,
                           const std::vector<InstMapping> &PCs,
                           Inst *LHS, bool &Negative,
                           InstContext &IC) override {
    return UnderlyingSolver->negative(BPCs, PCs, LHS, Negative, IC);
  }

  std::error_code abstractPrecondition(const BlockPCs &BPCs,
                  const std::vector<InstMapping> &PCs,
                  InstMapping &Mapping, InstContext &IC,
                  bool &FoundWeakest) override {
    return UnderlyingSolver->abstractPrecondition(BPCs, PCs, Mapping, IC, FoundWeakest);
  }

  std::error_code knownBits(const BlockPCs &BPCs,
                            const std::vector<InstMapping> &PCs,
                            Inst *LHS, KnownBits &Known,
                            InstContext &IC) override {
    return UnderlyingSolver->knownBits(BPCs, PCs, LHS, Known, IC);
  }

  std::error_code powerTwo(const BlockPCs &BPCs,
                           const std::vector<InstMapping> &PCs,
                           Inst *LHS, bool &PowerTwo,
                           InstContext &IC) override {
    return UnderlyingSolver->powerTwo(BPCs, PCs, LHS, PowerTwo, IC);
  }

  std::error_code nonZero(const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          Inst *LHS, bool &NonZero,
                          InstContext &IC) override {
    return UnderlyingSolver->nonZero(BPCs, PCs, LHS, NonZero, IC);
  }

  std::error_code signBits(const BlockPCs &BPCs,
                           const std::vector<InstMapping> &PCs,
                           Inst *LHS, unsigned &SignBits,
                           InstContext &IC) override {
    return UnderlyingSolver->signBits(BPCs, PCs, LHS, SignBits, IC);
  }

};

class ExternalCachingSolver : public Solver {
  std::unique_ptr<Solver> UnderlyingSolver;
  KVStore *KV;

public:
  ExternalCachingSolver(std::unique_ptr<Solver> UnderlyingSolver, KVStore *KV)
      : UnderlyingSolver(std::move(UnderlyingSolver)), KV(KV) {
  }

  std::error_code inferConst(const BlockPCs &BPCs,
                             const std::vector<InstMapping> &PCs,
                             Inst *LHS, Inst *&RHS,
                             std::set<Inst *> &ConstSet,
                             std::map<Inst *, llvm::APInt> &ResultMap,
                             InstContext &IC) override {
    return UnderlyingSolver->inferConst(BPCs, PCs, LHS, RHS, ConstSet, ResultMap, IC);
  }


  std::error_code infer(const BlockPCs &BPCs,
                        const std::vector<InstMapping> &PCs,
                        Inst *LHS, std::vector<Inst *> &RHSs,
                        bool AllowMultipleRHSs,
                        InstContext &IC) override {
    ReplacementContext Context;
    std::string LHSStr = GetReplacementLHSString(BPCs, PCs, LHS, Context);
    if (LHSStr.length() > MaxLHSSize)
      return std::make_error_code(std::errc::value_too_large);
    std::string S;
    if (KV->hGet(LHSStr, "rhs", S)) {
      if (DebugLevel > 3)
        llvm::errs() << "(external cache hit)\n";
      ++ExternalHits;
      if (S == "") {
        RHSs.clear();
      } else {
        std::string ES;
        ParsedReplacement R = ParseReplacementRHS(IC, "<cache>", S, Context, ES);
        if (ES != "")
          return std::make_error_code(std::errc::protocol_error);
        RHSs.emplace_back(R.Mapping.RHS);
      }
      return std::error_code();
    } else {
      ++ExternalMisses;
      if (DebugLevel > 3)
        llvm::errs() << "(external cache miss)\n";
      if (NoInfer) {
        RHSs.clear();
        KV->hSet(LHSStr, "noinfer", "");
        return std::error_code();
      }
      std::error_code EC = UnderlyingSolver->infer(BPCs, PCs, LHS, RHSs,
                                                   AllowMultipleRHSs, IC);
      std::string RHSStr;
      if (!EC && !RHSs.empty()) {
        // TODO: support multi RHSs caching
        RHSStr = GetReplacementRHSString(RHSs.front(), Context);
      }
      KV->hSet(LHSStr, "rhs", RHSStr);
      return EC;
    }
  }

  llvm::ConstantRange constantRange(const BlockPCs &BPCs,
                                    const std::vector<InstMapping> &PCs,
                                    Inst *LHS,
                                    InstContext &IC) override {
    return UnderlyingSolver->constantRange(BPCs, PCs, LHS, IC);
  }

  std::error_code isValid(InstContext &IC, const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          InstMapping Mapping, bool &IsValid,
                          std::vector<std::pair<Inst *, llvm::APInt>> *Model)
  override {
    // N.B. we decided that since the important clients have moved to infer(),
    // we'll no longer support external caching for isValid()
    return UnderlyingSolver->isValid(IC, BPCs, PCs, Mapping, IsValid, Model);
  }

  std::string getName() override {
    return UnderlyingSolver->getName() + " + external cache";
  }

  std::error_code testDemandedBits(const BlockPCs &BPCs,
                                   const std::vector<InstMapping> &PCs,
                                   Inst *LHS,
                                   std::map<std::string, APInt> &DBitsVect,
                                   InstContext &IC) override {
    return UnderlyingSolver->testDemandedBits(BPCs, PCs, LHS, DBitsVect, IC);
  }

  std::error_code nonNegative(const BlockPCs &BPCs,
                              const std::vector<InstMapping> &PCs,
                              Inst *LHS, bool &NonNegative,
                              InstContext &IC) override {
    return UnderlyingSolver->nonNegative(BPCs, PCs, LHS, NonNegative, IC);
  }

  std::error_code negative(const BlockPCs &BPCs,
                           const std::vector<InstMapping> &PCs,
                           Inst *LHS, bool &Negative,
                           InstContext &IC) override {
    return UnderlyingSolver->negative(BPCs, PCs, LHS, Negative, IC);
  }

  std::error_code abstractPrecondition(const BlockPCs &BPCs,
                  const std::vector<InstMapping> &PCs,
                  InstMapping &Mapping, InstContext &IC,
                  bool &FoundWeakest) override {
    return UnderlyingSolver->abstractPrecondition(BPCs, PCs, Mapping, IC, FoundWeakest);
  }

  std::error_code knownBits(const BlockPCs &BPCs,
                            const std::vector<InstMapping> &PCs,
                            Inst *LHS, KnownBits &Known,
                            InstContext &IC) override {
    return UnderlyingSolver->knownBits(BPCs, PCs, LHS, Known, IC);
  }

  std::error_code powerTwo(const BlockPCs &BPCs,
                           const std::vector<InstMapping> &PCs,
                           Inst *LHS, bool &PowerTwo,
                           InstContext &IC) override {
    return UnderlyingSolver->powerTwo(BPCs, PCs, LHS, PowerTwo, IC);
  }

  std::error_code nonZero(const BlockPCs &BPCs,
                          const std::vector<InstMapping> &PCs,
                          Inst *LHS, bool &NonZero,
                          InstContext &IC) override {
    return UnderlyingSolver->nonZero(BPCs, PCs, LHS, NonZero, IC);
  }

  std::error_code signBits(const BlockPCs &BPCs,
                           const std::vector<InstMapping> &PCs,
                           Inst *LHS, unsigned &SignBits,
                           InstContext &IC) override {
    return UnderlyingSolver->signBits(BPCs, PCs, LHS, SignBits, IC);
  }

};

}

namespace souper {

Solver::~Solver() {}

std::unique_ptr<Solver> createBaseSolver(
    std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout) {
  return std::unique_ptr<Solver>(new BaseSolver(std::move(SMTSolver), Timeout));
}

std::unique_ptr<Solver> createMemCachingSolver(
    std::unique_ptr<Solver> UnderlyingSolver) {
  return std::unique_ptr<Solver>(
      new MemCachingSolver(std::move(UnderlyingSolver)));
}

std::unique_ptr<Solver> createExternalCachingSolver(
    std::unique_ptr<Solver> UnderlyingSolver, KVStore *KV) {
  return std::unique_ptr<Solver>(
      new ExternalCachingSolver(std::move(UnderlyingSolver), KV));
}

}
