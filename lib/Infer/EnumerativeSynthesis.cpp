// Copyright 2018 The Souper Authors. All rights reserved.
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

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/CommandLine.h"
#include "souper/Infer/AliveDriver.h"
#include "souper/Infer/Z3Driver.h"
#include "souper/Infer/ConstantSynthesis.h"
#include "souper/Infer/EnumerativeSynthesis.h"
#include "souper/Infer/Pruning.h"

#include <queue>
#include <functional>
#include <set>

static const unsigned MaxTries = 30;

bool UseAlive;
extern unsigned DebugLevel;

using namespace souper;
using namespace llvm;

static const std::vector<Inst::Kind> UnaryOperators = {
  Inst::CtPop, Inst::BSwap, Inst::BitReverse, Inst::Cttz, Inst::Ctlz, Inst::Freeze
};

static const std::vector<Inst::Kind> BinaryOperators = {
  Inst::Add, Inst::Sub, Inst::Mul,
  Inst::UDiv, Inst::SDiv,
  Inst::URem, Inst::SRem,
  Inst::And, Inst::Or, Inst::Xor,
  Inst::Shl, Inst::AShr, Inst::LShr,
  Inst::Eq, Inst::Ne, Inst::Ult,
  Inst::Slt, Inst::Ule, Inst::Sle,
  Inst::SAddSat, Inst::UAddSat,
  Inst::SSubSat, Inst::USubSat,

  /* Overflow intrinsics are synthesized always with `extractvalue`.
   * Main overflow intrinsics like {S,U}{Add,Sub,Mul}WithOverflow below means synthesizing
   * extractvalue with index 0. On the other hand, synthesizing suboperations like {U,S}{Add,Sub,Mul}O
   * means synthesizing extractvalue with index 1. */
  Inst::SAddWithOverflow, Inst::UAddWithOverflow,
  Inst::SSubWithOverflow, Inst::USubWithOverflow,
  Inst::SMulWithOverflow, Inst::UMulWithOverflow,
  Inst::SAddO, Inst::UAddO, Inst::SSubO,
  Inst::USubO, Inst::SMulO, Inst::UMulO
};

static const std::vector<Inst::Kind> TernaryOperators = {
  Inst::Select, Inst::FShl, Inst::FShr
};

namespace {
  static cl::opt<unsigned> MaxNumInstructions("souper-enumerative-synthesis-max-instructions",
    cl::desc("Maximum number of instructions to synthesize (default=0)."),
    cl::init(0));
  static cl::opt<unsigned> MaxV("souper-enumerative-synthesis-max-verification-load",
    cl::desc("Maximum number of guesses verified at once (default=300)."),
    cl::init(300));
  static cl::opt<bool, /*ExternalStorage=*/true>
    AliveFlagParser("souper-use-alive", cl::desc("Use Alive2 as the backend"),
    cl::Hidden, cl::location(UseAlive), cl::init(false));
  static cl::opt<bool> UseNativeZ3("souper-in-process-z3-synthesis",
    cl::desc("Use the Z3 C++ API for synthesis (default=false)"),
    cl::init(false));
  static cl::opt<bool> LSBPruning("souper-lsb-pruning",
    cl::desc("Try to prune guesses by looking for a difference in LSB"),
    cl::init(false));
  static cl::opt<bool> EnableDataflowPruning("souper-dataflow-pruning",
    cl::desc("Enable pruning based on dataflow analysis (default=false)"),
    cl::init(false));
  static cl::opt<bool> SynthesisConstWithCegisLoop("souper-synthesis-const-with-cegis",
    cl::desc("Synthesis constants with CEGIS (default=false)"),
    cl::init(true));
  static cl::opt<bool> DoubleCheckWithAlive("souper-double-check",
    cl::desc("Double check synthesis result with alive (default=false)"),
    cl::init(false));
  static cl::opt<bool> SkipSolver("souper-enumerative-synthesis-skip-solver",
    cl::desc("Skip refinement check after generating guesses (default=false)"),
    cl::init(false));
  static cl::opt<bool> IgnoreCost("souper-enumerative-synthesis-ignore-cost",
    cl::desc("Ignore cost of RHSs -- just generate them (default=false)"),
    cl::init(false));
  static cl::opt<unsigned> MaxLHSCands("souper-max-lhs-cands",
    cl::desc("Gather at most this many values from a LHS to use as synthesis inputs (default=10)"),
    cl::init(10));
  static cl::opt<unsigned> CostFudge("souper-enumerative-synthesis-cost-fudge",
    cl::desc("Generate guesses costing LHS + N (default=0)"),
    cl::init(0));
  static cl::opt<bool> OnlyInferI1("souper-only-infer-i1",
    cl::desc("Only infer integer constants with width 1 (default=false)"),
    cl::init(false));
  static cl::opt<bool> OnlyInferIN("souper-only-infer-iN",
    cl::desc("Only infer integer constants (default=false)"),
    cl::init(false));
  static cl::opt<bool> TryShrinkConsts("souper-shrink-consts",
    cl::desc("Try to shrink constants (defaults=false)"),
    cl::init(false));
}

// TODO
// tune the constants at the top of the file
// constant synthesis
//   try to first make small constants? -128-255?
// multiple instructions
//   make a fresh constant per new binary/ternary instruction
// call solver in batches -- need to work on batch size
//   tune batch size -- can be a lot bigger for simple RHSs
//   or look for feedback from solver, timeouts etc.
// make sure path conditions work as expected
// once an optimization works we can try adding UB qualifiers on the RHS
//   probably almost as good as synthesizing these directly
// prune a subtree once it becomes clear it isn't a cost win
// aggressively prune dumb instructions
//   which logical operations are equivalent at 1 bit? (include icmps in this)
// add constraints guarding against synthesizing dumb constants
//   add x, 0; sub x, 0; shift x, 0; mul x, 0; mul x, 1, mul x, 2
//   shift left by 1
// aggressively prune dumb instruction sequences
//   trunc of trunc, sext of sext, zext of zext
//   trunc to i1 vs. %w = and %v, 1; %x = icmp ne %w, 0
//   a bloom filter or something
//   use the solver to recognize non-minimal instructions / instruction sequences?
// we want to avoid guessing code that's already there on the LHS
//   hashing?
// test against CEGIS with LHS components
// test the width matching stuff
// take outside uses into account -- only in the cost model?
// experiment with synthesizing at reduced bitwidth, then expanding the result
// aggressively avoid calling into the solver

void addGuess(Inst *RHS, unsigned TargetWidth, InstContext &IC, int MaxCost,
              std::vector<Inst *> &Guesses, int &TooExpensive) {
  if (TargetWidth > RHS->Width) {
    auto NSExt = IC.getInst(Inst::SExt, TargetWidth, { RHS });
    auto NZExt = IC.getInst(Inst::ZExt, TargetWidth, { RHS });
    addGuess(NSExt, TargetWidth, IC, MaxCost, Guesses, TooExpensive);
    addGuess(NZExt, TargetWidth, IC, MaxCost, Guesses, TooExpensive);
  } else if (TargetWidth < RHS->Width) {
    auto NTrunc = IC.getInst(Inst::Trunc, TargetWidth, { RHS });
    addGuess(NTrunc, TargetWidth, IC, MaxCost, Guesses, TooExpensive);
  } else {
    if (IgnoreCost || souper::cost(RHS) < MaxCost)
      Guesses.push_back(RHS);
    else
      TooExpensive++;
  }
}

// Does a short-circuiting AND operation
PruneFunc MkPruneFunc(std::vector<PruneFunc> Funcs) {
  return [Funcs](Inst *I, std::vector<Inst *> &RI) {
    for (auto F : Funcs)
      if (!F(I, RI))
        return false;
    return true;
  };
}

bool CountPrune(Inst *I, std::vector<Inst *> &ReservedInsts, std::set<Inst*> Visited) {
  return !(souper::countHelper(I, Visited) > MaxNumInstructions);
}

//TODO(manasij/zhengyang) souper::cost needs a caching layer
template <typename Container>
void sortGuesses(Container &Guesses) {
  // One of the real advantages of enumerative synthesis vs
  // CEGIS is that we can synthesize in precisely increasing cost
  // order, and not try to somehow teach the solver how to do that
  std::stable_sort(Guesses.begin(), Guesses.end(),
                   [](Inst *a, Inst *b) -> bool {
                     return souper::cost(a) < souper::cost(b);
                   });
}

using CallbackType = std::function<bool(Inst *)>;

bool getGuesses(const std::set<Inst *> &Inputs,
                int Width, int LHSCost,
                InstContext &IC, Inst *PrevInst, Inst *PrevSlot,
                int &TooExpensive,
                PruneFunc prune, CallbackType Generate) {

  std::vector<Inst *> unaryHoleUsers;
  findInsts(PrevInst, unaryHoleUsers, [PrevSlot](Inst *I) {
    return I->Ops.size() == 1 && I->Ops[0] == PrevSlot;
  });

  std::vector<Inst::Kind> unaryExclList;
  if (unaryHoleUsers.size() == 1 &&
      (unaryHoleUsers[0]->K == Inst::Ctlz ||
       unaryHoleUsers[0]->K == Inst::Cttz ||
       unaryHoleUsers[0]->K == Inst::BitReverse ||
       unaryHoleUsers[0]->K == Inst::CtPop)) {
    unaryExclList.push_back(Inst::BitReverse);
  }

  // disable generating freeze of freeze
  if (unaryHoleUsers.size() == 1 && unaryHoleUsers[0]->K == Inst::Freeze)
    unaryExclList.push_back(Inst::Freeze);

  std::vector<Inst *> PartialGuesses;
  std::vector<Inst *> Comps(Inputs.begin(), Inputs.end());

  // Conversion Operators
  for (auto Comp : Comps)
    if (Comp->Width != Width)
      addGuess(Comp, Width, IC, LHSCost, PartialGuesses, TooExpensive);

  Inst *I1 = IC.getReservedInst();
  Comps.push_back(I1);

  // Unary Operators
  for (auto K : UnaryOperators) {
    if (std::find(unaryExclList.begin(), unaryExclList.end(), K) != unaryExclList.end())
      continue;

    if (K != Inst::Freeze && Width <= 1)
      continue;

    for (auto Comp : Comps) {
      if (K == Inst::BSwap && Width % 16 != 0)
        continue;

      if (Comp->K == Inst::ReservedInst) {
        auto V = IC.createHole(Width);
        auto N = IC.getInst(K, Width, { V });
        addGuess(N, Width, IC, LHSCost, PartialGuesses, TooExpensive);
        continue;
      }

      if (Comp->Width != Width)
        continue;

      // Prune: unary operation on constant
      if (Comp->K == Inst::ReservedConst)
        continue;

      auto N = IC.getInst(K, Width, { Comp });
      addGuess(N, Width, IC, LHSCost, PartialGuesses, TooExpensive);
    }
  }

  // reservedinst and reservedconsts starts with width 0
  Inst *C1 = IC.getReservedConst();
  Comps.push_back(C1);
  Inst *I2 = IC.getReservedInst();
  Comps.push_back(I2);

  for (auto K : BinaryOperators) {

    // PRUNE: i1 is a special case for a number of operators
    if (Width == 1 &&
        (// these become trivial
         Inst::isDivRem(K) || Inst::isShift(K) ||
         // these canonicalize to "xor"
         K == Inst::Add || K == Inst::Sub || K == Inst::Ne ||
         // canonicalizes to "and"
         K == Inst::Mul ||
         // i1 versions of these do not tend to codegen well
         K == Inst::SAddSat || K == Inst::UAddSat ||
         K == Inst::SSubSat || K == Inst::USubSat ||
         K == Inst::SAddWithOverflow || K == Inst::UAddWithOverflow ||
         K == Inst::SSubWithOverflow || K == Inst::USubWithOverflow ||
         K == Inst::SMulWithOverflow || K == Inst::UMulWithOverflow)) {
      continue;
    }

    for (auto I = Comps.begin(); I != Comps.end(); ++I) {
      // Prune: only one of (mul x, C), (mul C, x) is allowed
      if ((Inst::isCommutative(K) || Inst::isOverflowIntrinsicMain(K) ||
           Inst::isOverflowIntrinsicSub(K)) && (*I)->K == Inst::ReservedConst)
        continue;

      // Prune: I1 should only be the first argument
      if ((*I)->K == Inst::ReservedInst && (*I) != I1)
        continue;

      // PRUNE: don't try commutative operators both ways
      auto Start = (Inst::isCommutative(K) ||
		      Inst::isOverflowIntrinsicMain(K) ||
		      Inst::isOverflowIntrinsicSub(K)) ? I : Comps.begin();
      for (auto J = Start; J != Comps.end(); ++J) {
        // Prune: I2 should only be the second argument
        if ((*J)->K == Inst::ReservedInst && (*J) != I2)
          continue;

        // PRUNE: never useful to cmp, sub, and, or, xor, div, rem,
        // usub.sat, ssub.sat, ashr, lshr a value against itself
        // Also do it for sub.overflow -- no sense to check for overflow when results = 0
        if ((*I == *J) && (Inst::isCmp(K) || K == Inst::And || K == Inst::Or ||
                           K == Inst::Xor || K == Inst::Sub || K == Inst::UDiv ||
                           K == Inst::SDiv || K == Inst::SRem || K == Inst::URem ||
                           K == Inst::USubSat || K == Inst::SSubSat ||
                           K == Inst::AShr || K == Inst::LShr || K == Inst::SSubWithOverflow ||
                           K == Inst::USubWithOverflow || K == Inst::SSubO || K == Inst::USubO))
          continue;

        // PRUNE: never operate on two constants
        if ((*I)->K == Inst::ReservedConst && (*J)->K == Inst::ReservedConst)
          continue;

        // see if we need to make a var representing a constant
        // that we don't know yet

        Inst *V1, *V2;
        if (Inst::isCmp(K)) {

          if ((*I)->Width == 0 && (*J)->Width == 0) {
            // TODO: support (cmp hole, hole);
            // TODO: support (cmp hole, c) and (cmp c, hole)
            continue;
          }

          if ((*I)->Width == 0) {
            if ((*I)->K == Inst::ReservedConst) {
              // (cmp const, comp)
              V1 = IC.createSynthesisConstant((*J)->Width, (*I)->SynthesisConstID);
            } else if ((*I)->K == Inst::ReservedInst) {
              // (cmp hole, comp)
              V1 = IC.createHole((*J)->Width);
            }
          } else {
            V1 = *I;
          }

          if ((*J)->Width == 0) {
            if ((*J)->K == Inst::ReservedConst) {
              // (cmp comp, const)
              V2 = IC.createSynthesisConstant((*I)->Width, (*J)->SynthesisConstID);
            } else if ((*J)->K == Inst::ReservedInst) {
              // (cmp comp, hole)
              V2 = IC.createHole((*I)->Width);
            }
          } else {
            V2 = *J;
          }
        } else {
          if ((*I)->K == Inst::ReservedConst) {
            // (binop const, comp)
            V1 = IC.createSynthesisConstant(Width, (*I)->SynthesisConstID);
          } else if ((*I)->K == Inst::ReservedInst) {
            // (binop hole, comp)
            V1 = IC.createHole(Width);
          } else {
            V1 = *I;
          }

          if ((*J)->K == Inst::ReservedConst) {
            // (binop comp, const)
            V2 = IC.createSynthesisConstant(Width, (*J)->SynthesisConstID);
          } else if ((*J)->K == Inst::ReservedInst) {
            // (binop comp, hole)
            V2 = IC.createHole(Width);
          } else {
            V2 = *J;
          }
        }

        if (V1->Width != V2->Width)
          continue;

        if (!(Inst::isCmp(K) || Inst::isOverflowIntrinsicSub(K)) && V1->Width != Width)
          continue;

        // PRUNE: don't synthesize sub x, C since this is covered by add x, -C
        if (K == Inst::Sub && V2->K == Inst::Var && V2->SynthesisConstID != 0)
          continue;

        Inst *N = nullptr;
        if (Inst::isOverflowIntrinsicMain(K)) {
          auto Comp0 = IC.getInst(Inst::getBasicInstrForOverflow(K), V1->Width, {V1, V2});
          auto Comp1 = IC.getInst(Inst::getOverflowComplement(K), 1, {V1, V2});
          auto Orig = IC.getInst(K, V1->Width + 1, {Comp0, Comp1});
          N = IC.getInst(Inst::ExtractValue, V1->Width, {Orig, IC.getConst(llvm::APInt(32, 0))});
        }
        else if (Inst::isOverflowIntrinsicSub(K)) {
          auto Comp0 = IC.getInst(Inst::getBasicInstrForOverflow(Inst::getOverflowComplement(K)),
                                  V1->Width, {V1, V2});
          auto Comp1 = IC.getInst(K, 1, {V1, V2});
          auto Orig = IC.getInst(Inst::getOverflowComplement(K), V1->Width + 1, {Comp0, Comp1});
          N = IC.getInst(Inst::ExtractValue, 1, {Orig, IC.getConst(llvm::APInt(32, 1))});
        }
        else {
          N = IC.getInst(K, Inst::isCmp(K) ? 1 : Width, {V1, V2});
        }

        addGuess(N, Width, IC, LHSCost, PartialGuesses, TooExpensive);
      }
    }
  }

  // Deal with ternary instructions separately, since some guesses might
  // need two reserved per instruction
  Inst *C2 = IC.getReservedConst();
  Comps.push_back(C2);
  Inst *C3 = IC.getReservedConst();
  Comps.push_back(C3);
  Inst *I3 = IC.getReservedInst();
  Comps.push_back(I3);

  for (auto Op : TernaryOperators) {
    for (auto I : Comps) {
      if (I->K == Inst::ReservedInst && I != I1)
        continue;
      if (I->K == Inst::ReservedConst && I != C1)
        continue;

      // (select c, x, y)
      // PRUNE: a select's control input should never be constant
      if (Op == Inst::Select && I->K == Inst::ReservedConst)
        continue;

      // PRUNE: don't generate an i1 using funnel shift
      if (Width == 1 && (Op == Inst::FShr || Op == Inst::FShl))
        continue;

      Inst *V1;
      if (I->K == Inst::ReservedConst) {
        V1 = IC.createSynthesisConstant(Width, I->SynthesisConstID);
      } else if (I->K == Inst::ReservedInst) {
        V1 = IC.createHole(Op == Inst::Select ? 1 : Width);
      } else {
        V1 = I;
      }

      if (Op == Inst::Select && V1->Width != 1)
        continue;
      if (Op != Inst::Select && V1->Width != Width)
        continue;

      for (auto J : Comps) {
        if (J->K == Inst::ReservedInst && J != I2)
          continue;
        if (J->K == Inst::ReservedConst && J != C2)
          continue;

        Inst *V2;
        if (J->K == Inst::ReservedConst) {
          V2 = IC.createSynthesisConstant(Width, J->SynthesisConstID);
        } else if (J->K == Inst::ReservedInst) {
          V2 = IC.createHole(Width);
        } else {
          V2 = J;
        }

        if (V2->Width != Width)
          continue;

        for (auto K : Comps) {
          if (K->K == Inst::ReservedInst && K != I3)
            continue;
          if (K->K == Inst::ReservedConst && K != C3)
            continue;

          // PRUNE: ter-op c, c, c
          if (I->K == Inst::ReservedConst && J->K == Inst::ReservedConst &&
              K->K == Inst::ReservedConst)
            continue;

          // PRUNE: (select cond, x, x)
          if (Op == Inst::Select && J == K)
            continue;

          Inst *V3;
          if (K->K == Inst::ReservedConst) {
            V3 = IC.createSynthesisConstant(Width, K->SynthesisConstID);
          } else if (K->K == Inst::ReservedInst) {
            V3 = IC.createHole(Width);
          } else {
            V3 = K;
          }

          if (V2->Width != V3->Width)
            continue;

          auto N = IC.getInst(Op, Width, {V1, V2, V3});
          addGuess(N, Width, IC, LHSCost, PartialGuesses, TooExpensive);
        }
      }
    }
  }

  // FIXME: This is a bit heavy-handed. Find a way to eliminate this sorting.
  sortGuesses(PartialGuesses);

  for (auto I : PartialGuesses) {
    Inst *JoinedGuess;
    // if it is the first time the function getGuesses() gets called, then
    // leave it as the root and do not plug it to any other insts
    if (!PrevInst)
      JoinedGuess = I;
    else {
      // plugin the new guess I to PrevInst
      std::map<Inst *, Inst *> InstCache;
      JoinedGuess = instJoin(PrevInst, PrevSlot, I, InstCache, IC);
    }

    // get all empty slots from the newly plugged inst
    std::vector<Inst *> CurrSlots;
    getHoles(JoinedGuess, CurrSlots);
    //FIXME: This is inefficient, to do for each symbolic and concrete candidate

    // if no empty slot, then push the guess to the result list
    if (CurrSlots.empty()) {
      std::vector<Inst *> empty;
      if (prune(JoinedGuess, empty)) {
        std::vector<Inst *> ConcreteTypedGuesses;
        addGuess(JoinedGuess, JoinedGuess->Width, IC, LHSCost, ConcreteTypedGuesses, TooExpensive);
        for (auto &&Guess : ConcreteTypedGuesses) {
          if (!Generate(Guess)) {
            return false;
          }
        }
      }
      continue;
    }

    // if there exist empty slots, then call getGuesses() recursively
    // and fill the empty slots
    if (prune(JoinedGuess, CurrSlots)) {
      // TODO: replace this naive hole selection with some better algorithms
      if (!getGuesses(Inputs, CurrSlots.front()->Width,
                      LHSCost, IC, JoinedGuess,
                      CurrSlots.front(), TooExpensive, prune, Generate)) {
        return false;
      }
    }
  }
  return true;
}

Inst *findConst(souper::Inst *I,
                std::set<const Inst *> &Visited) {
  if (I->K == Inst::Var && I->SynthesisConstID != 0) {
    return I;
  } else {
    for (auto &&Op : I->Ops) {
      if (Visited.find(Op) == Visited.end()) {
        auto Ret = findConst(Op, Visited);
        if (Ret)
          return Ret;
      }
    }
    Visited.insert(I);
  }
  return nullptr;
}

bool exceeds64Bits(const Inst *I, std::set<const Inst *> &Visited) {
  if (I->Width > 64) {
    return true;
  } else {
    for (auto &&Op : I->Ops) {
      if (Visited.find(Op) == Visited.end()) {
        if (exceeds64Bits(Op, Visited))
          return true;
      }
    }
    Visited.insert(I);
  }
  return false;
}

bool canDifferInLSB(SynthesisContext &SC, Inst *RHSGuess) {
  Inst *LHSOne = SC.IC.getConst(llvm::APInt(SC.LHS->Width, 1));
  Inst *NewLHS = SC.IC.getInst(Inst::And, SC.LHS->Width, {SC.LHS, LHSOne});
  Inst *RHSOne = SC.IC.getConst(llvm::APInt(RHSGuess->Width, 1));
  Inst *NewRHS = SC.IC.getInst(Inst::And, RHSGuess->Width, {RHSGuess, RHSOne});
  // TODO: Experiment with larger masks: 3, 7, MSB, etc.

  InstMapping NewMapping{NewLHS, NewRHS};

  auto Query = BuildQuery(SC.IC, SC.BPCs, SC.PCs, NewMapping, 0, 0);

  bool QueryIsSat;
  auto EC = SC.SMTSolver->isSatisfiable(Query, QueryIsSat, 0, 0, SC.Timeout);
  if (EC) {
    if (DebugLevel > 1)
      llvm::errs() << "Solver error in LSB pruning!\n";
    return false;
  }
  return QueryIsSat;
}

std::error_code synthesizeWithAlive(SynthesisContext &SC, std::vector<Inst *> &RHSs,
                                    const std::vector<souper::Inst *> &Guesses) {
  std::error_code EC;
  std::map<Inst *, Inst *> InstCache;
  std::map<Block *, Block *> BlockCache;
  std::set<const Inst *> Visited;
  if (exceeds64Bits(SC.LHS, Visited))
    llvm::report_fatal_error("LHS exceeds 64 bits");

  Inst *Ante = SC.IC.getConst(APInt(1, true));
  for (auto PC : SC.PCs ) {
    Inst *Eq = SC.IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
    Ante = SC.IC.getInst(Inst::And, 1, {Ante, Eq});
  }

  AliveDriver Verifier(SC.LHS, Ante, SC.IC);
  Inst *RHS;
  for (auto &&G : Guesses) {
    std::set<const Inst *> Visited;
    auto C = findConst(G, Visited);
    if (!C) {
      if (Verifier.verify(G)) {
        RHS = G;
      } else {
        continue;
      }
    } else {
      if (SynthesisConstWithCegisLoop) {
        auto ConstMap = Verifier.synthesizeConstantsWithCegis(G, SC.IC);
        if (ConstMap.empty())
          continue;
        auto GWithC = getInstCopy(G, SC.IC, InstCache, BlockCache, &ConstMap,
                                  /*CloneVars=*/false);
        RHS = GWithC;
      } else {
        auto ConstMap = Verifier.synthesizeConstants(G);
        // TODO: Counterexample guided loop or UB constraints in query

        auto GWithC = getInstCopy(G, SC.IC, InstCache, BlockCache, &ConstMap,
                                  /*CloneVars=*/false);
        if (Verifier.verify(GWithC)) {
          RHS = GWithC;
        } else {
          continue;
        }
      }
    }
    assert (RHS);
    RHSs.emplace_back(RHS);
    if (!SC.CheckAllGuesses)
      return EC;

    if (DebugLevel > 3) {
      llvm::outs() << "; result " << RHSs.size() << ":\n";
      ReplacementContext RC;
      RC.printInst(RHS, llvm::outs(), false);
      llvm::outs() << "\n";
    }
  }
  return EC;
}

std::error_code isConcreteCandidateSat(SynthesisContext &SC, Inst *RHSGuess, bool &IsSat) {
  std::error_code EC;
  InstMapping Mapping(SC.LHS, RHSGuess);

  std::string Query2 = BuildQuery(SC.IC, SC.BPCs, SC.PCs, Mapping, 0, 0);

  EC = SC.SMTSolver->isSatisfiable(Query2, IsSat, 0, 0, SC.Timeout);
  if (EC && DebugLevel > 1) {
    llvm::errs() << "verification query failed!\n";
  }
  return EC;
}

std::error_code synthesizeWithKLEE(SynthesisContext &SC, std::vector<Inst *> &RHSs,
                                   const std::vector<souper::Inst *> &Guesses) {
  std::error_code EC;

  // find the valid one
  int GuessIndex = -1;

  if (DebugLevel > 2) {
    llvm::errs() << "\n--------------------- synthesizeWithKLEE ---------------------------\n";
    ReplacementContext Context;
    auto S = GetReplacementLHSString(SC.BPCs, SC.PCs,
                                     SC.LHS, Context);
    llvm::errs() << S << "\n";
    llvm::errs() << "there are " << Guesses.size() << " guesses to check\n";
  }

  for (auto I : Guesses) {
    GuessIndex++;
    if (DebugLevel > 2) {
      llvm::errs() << "\n--------------------------------\n";
      llvm::errs() << "guess " << GuessIndex << "\n\n";
      ReplacementContext RC;
      RC.printInst(I, llvm::errs(), /*printNames=*/true);
      llvm::errs() << "\n";
      llvm::errs() << "Cost = " << souper::cost(I, /*IgnoreDepsWithExternalUses=*/true) << "\n";
    }

    Inst *RHS = nullptr;
    std::set<Inst *> ConstSet;
    std::map <Inst *, llvm::APInt> ResultConstMap;
    souper::getConstants(I, ConstSet);
    bool GuessHasConstant = !ConstSet.empty();
    if (!GuessHasConstant) {
      bool IsSAT;

      EC = isConcreteCandidateSat(SC, I, IsSAT);
      if (EC) {
        if (DebugLevel > 0)
          llvm::errs() << "OOPS: error from isConcreteCanddiateSat()\n";
        continue;
      }
      if (IsSAT) {
        if (DebugLevel > 3)
          llvm::errs() << "this guess doesn't work\n";
        continue;
      } else {
        if (DebugLevel > 3)
          llvm::errs() << "query is UNSAT, guess works\n";
        RHS = I;
      }
    } else {
      // guess has constant(s)
      ConstantSynthesis CS;
      EC = CS.synthesize(SC.SMTSolver, SC.BPCs, SC.PCs, InstMapping (SC.LHS, I), ConstSet,
                         ResultConstMap, SC.IC, /*MaxTries=*/MaxTries, SC.Timeout,
                         /*AvoidNops=*/true);
      if (ResultConstMap.empty())
        continue;
      std::map<Inst *, Inst *> InstCache;
      std::map<Block *, Block *> BlockCache;
      RHS = getInstCopy(I, SC.IC, InstCache, BlockCache, &ResultConstMap, false, false);
    }

    assert(RHS);

    if (DoubleCheckWithAlive) {
      if (isTransformationValid(SC.LHS, RHS, SC.PCs, SC.BPCs, SC.IC)) {
        if (DebugLevel > 3) {
          llvm::errs() << "Transformation verified by alive.\n";
        }
      } else {
        if (DebugLevel > 1) {
          llvm::errs() << "Transformation could not be verified by alive.\n";
          ReplacementContext RC;
          auto str = RC.printInst(SC.LHS, llvm::errs(), /*printNames=*/true);
          llvm::errs() << "infer " << str << "\n";
          str = RC.printInst(RHS, llvm::errs(), /*printNames=*/true);
          llvm::errs() << "result " << str << "\n";
        }
        RHS = nullptr;
      }
    }

    if (TryShrinkConsts) {
      // FIXME shrink constants properly, this is a placeholder where we
      // just see if we can replace every constant with zero
      // TODO(manasij) : Implement binary search, involve alive only when we find a solution
      if (RHS && !ResultConstMap.empty() && DoubleCheckWithAlive) {
        std::map <Inst *, llvm::APInt> ZeroConstMap;
        for (auto it : ResultConstMap) {
          auto I = it.first;
          ZeroConstMap[I] = llvm::APInt(I->Width, 0);
        }
        std::map<Inst *, Inst *> InstCache;
        std::map<Block *, Block *> BlockCache;
        auto newRHS = getInstCopy(I, SC.IC, InstCache, BlockCache, &ZeroConstMap, false, false);
        if (isTransformationValid(SC.LHS, newRHS, SC.PCs, SC.BPCs, SC.IC))
          RHS = newRHS;
      }
    }

    if (RHS) {
      RHSs.emplace_back(RHS);
      if (!SC.CheckAllGuesses) {
        if (DebugLevel > 2)
          llvm::errs() << "\n------ normal exit of synthesizeWithKLEE with 1 result ------\n";
        return EC;
      }
      if (DebugLevel > 3) {
        llvm::outs() << "; result " << RHSs.size() << ":\n";
        ReplacementContext RC;
        RC.printInst(RHS, llvm::outs(), true);
        llvm::outs() << "\n";
      }
    }
  }

  if (DebugLevel > 2)
    llvm::errs() << "\n------ normal exit of synthesizeWithKLEE ----------------\n";
  return EC;
}

std::error_code synthesizeWithZ3(SynthesisContext &SC, std::vector<Inst *> &RHSs,
                                 const std::vector<souper::Inst *> &Guesses) {
  std::error_code EC;

  // find the valid one
  int GuessIndex = -1;

  if (DebugLevel > 2) {
    llvm::errs() << "\n--------------------- synthesizeWithZ3 ---------------------------\n";
    ReplacementContext Context;
    auto S = GetReplacementLHSString(SC.BPCs, SC.PCs,
                                     SC.LHS, Context);
    llvm::errs() << S << "\n";
    llvm::errs() << "there are " << Guesses.size() << " guesses to check\n";
  }

  for (auto I : Guesses) {
    GuessIndex++;
    if (DebugLevel > 2) {
      llvm::errs() << "\n--------------------------------\n";
      llvm::errs() << "guess " << GuessIndex << "\n\n";
      ReplacementContext RC;
      RC.printInst(I, llvm::errs(), /*printNames=*/true);
      llvm::errs() << "\n";
      llvm::errs() << "Cost = " << souper::cost(I, /*IgnoreDepsWithExternalUses=*/true) << "\n";
    }

    Inst *RHS = nullptr;
    std::set<Inst *> ConstSet;
    std::map <Inst *, llvm::APInt> ResultConstMap;
    souper::getConstants(I, ConstSet);
    bool GuessHasConstant = !ConstSet.empty();
    if (!GuessHasConstant) {
      if (isTransformationValidZ3(SC.LHS, I, SC.PCs, SC.BPCs, SC.IC, SC.Timeout)) {
        if (DebugLevel > 3)
          llvm::errs() << "query is UNSAT, guess works\n";
        RHS = I;
      } else {
        if (DebugLevel > 3)
          llvm::errs() << "this guess doesn't work\n";
        continue;
      }
    } else {
      // guess has constant(s)
      ConstantSynthesisZ3 CS;
      EC = CS.synthesize(SC.SMTSolver, SC.BPCs, SC.PCs, InstMapping (SC.LHS, I), ConstSet,
                         ResultConstMap, SC.IC, /*MaxTries=*/MaxTries, SC.Timeout,
                         /*AvoidNops=*/true);
      if (ResultConstMap.empty())
        continue;
      std::map<Inst *, Inst *> InstCache;
      std::map<Block *, Block *> BlockCache;
      RHS = getInstCopy(I, SC.IC, InstCache, BlockCache, &ResultConstMap, false, false);
    }

    assert(RHS);

    if (DoubleCheckWithAlive) {
      if (isTransformationValid(SC.LHS, RHS, SC.PCs, SC.BPCs, SC.IC)) {
        if (DebugLevel > 3) {
          llvm::errs() << "Transformation verified by alive.\n";
        }
      } else {
        if (DebugLevel > 1) {
          llvm::errs() << "Transformation could not be verified by alive.\n";
          ReplacementContext RC;
          auto str = RC.printInst(SC.LHS, llvm::errs(), /*printNames=*/true);
          llvm::errs() << "infer " << str << "\n";
          str = RC.printInst(RHS, llvm::errs(), /*printNames=*/true);
          llvm::errs() << "result " << str << "\n";
        }
        RHS = nullptr;
      }
    }

    if (TryShrinkConsts) {
      // FIXME shrink constants properly, this is a placeholder where we
      // just see if we can replace every constant with zero
      // TODO(manasij) : Implement binary search, involve alive only when we find a solution
      if (RHS && !ResultConstMap.empty() && DoubleCheckWithAlive) {
        std::map <Inst *, llvm::APInt> ZeroConstMap;
        for (auto it : ResultConstMap) {
          auto I = it.first;
          ZeroConstMap[I] = llvm::APInt(I->Width, 0);
        }
        std::map<Inst *, Inst *> InstCache;
        std::map<Block *, Block *> BlockCache;
        auto newRHS = getInstCopy(I, SC.IC, InstCache, BlockCache, &ZeroConstMap, false, false);
        if (isTransformationValid(SC.LHS, newRHS, SC.PCs, SC.BPCs, SC.IC))
          RHS = newRHS;
      }
    }

    if (RHS) {
      RHSs.emplace_back(RHS);
      if (!SC.CheckAllGuesses) {
        if (DebugLevel > 2)
          llvm::errs() << "\n------ normal exit of synthesizeWithZ3 with 1 result ------\n";
        return EC;
      }
      if (DebugLevel > 3) {
        llvm::outs() << "; result " << RHSs.size() << ":\n";
        ReplacementContext RC;
        RC.printInst(RHS, llvm::outs(), true);
        llvm::outs() << "\n";
      }
    }
  }

  if (DebugLevel > 2)
    llvm::errs() << "\n------ normal exit of synthesizeWithZ3 ----------------\n";
  return EC;
}


std::error_code verify(SynthesisContext &SC, std::vector<Inst *> &RHSs,
                       const std::vector<souper::Inst *> &Guesses) {
  std::error_code EC;
  if (SkipSolver || Guesses.empty())
    return EC;

  if (UseNativeZ3) {
    return synthesizeWithZ3(SC, RHSs, Guesses);
  } else if (UseAlive) {
    return synthesizeWithAlive(SC, RHSs, Guesses);
  } else {
    return synthesizeWithKLEE(SC, RHSs, Guesses);
  }
}

std::error_code
EnumerativeSynthesis::synthesize(SMTLIBSolver *SMTSolver,
                                const BlockPCs &BPCs,
                                const std::vector<InstMapping> &PCs,
                                Inst *LHS, std::vector<Inst *> &RHSs,
                                bool CheckAllGuesses, InstContext &IC, unsigned Timeout) {
  if ((OnlyInferI1 || OnlyInferIN) && MaxNumInstructions >= 1)
    llvm::report_fatal_error("Sorry, it is an error to synthesize >= 1 instructions "
                             "in integer-only mode");
  if (OnlyInferI1 && OnlyInferIN)
    llvm::report_fatal_error("Sorry, it is an error to specify synthesizing both only "
                             "i1 and only iN values");
  SynthesisContext SC{IC, SMTSolver, LHS, getUBInstCondition(SC.IC, SC.LHS),
      PCs, BPCs, CheckAllGuesses, Timeout};
  std::error_code EC;
  std::set<Inst *> Cands;
  findCands(SC.LHS, Cands, /*WidthMustMatch=*/false, /*FilterVars=*/false, 1 + MaxLHSCands);
  if (DebugLevel > 1)
    llvm::errs() << "got " << Cands.size() << " candidates from LHS\n";
  for (auto PC : SC.PCs)
    findCands(PC.LHS, Cands, /*WidthMustMatch=*/false, /*FilterVars=*/false, 1 + MaxLHSCands);
  if (DebugLevel > 1)
    llvm::errs() << "got " << Cands.size() << " candidates from LHS + PCs\n";
  for (auto BPC : SC.BPCs)
    findCands(BPC.PC.LHS, Cands, /*WidthMustMatch=*/false, /*FilterVars=*/false, 1 + MaxLHSCands);
  if (DebugLevel > 1)
    llvm::errs() << "got " << Cands.size() << " candidates from LHS + PCs + BPCs\n";
  // do not use LHS itself as a candidate
  Cands.erase(SC.LHS);

  int LHSCost = souper::cost(SC.LHS, /*IgnoreDepsWithExternalUses=*/true) + CostFudge;
  int TooExpensive = 0;

  std::vector<Inst *> Inputs;
  findVars(SC.LHS, Inputs);
  PruningManager DataflowPruning(SC, Inputs, DebugLevel);

  std::set<Inst*> Visited(Cands.begin(), Cands.end());

  // Cheaper tests go first
  std::vector<PruneFunc> PruneFuncs = { [&Visited](Inst *I, std::vector<Inst*> &ReservedInsts)  {
    return CountPrune(I, ReservedInsts, Visited);
  }};
  if (EnableDataflowPruning) {
    DataflowPruning.init();
    PruneFuncs.push_back(DataflowPruning.getPruneFunc());
  }
  auto PruneCallback = MkPruneFunc(PruneFuncs);

  std::vector<Inst *> Guesses;

  auto Generate = [&SC, &Guesses, &RHSs, &EC](Inst *Guess) {
    Guesses.push_back(Guess);
    if (Guesses.size() >= MaxV && !SkipSolver) {
      sortGuesses(Guesses);
      EC = verify(SC, RHSs, Guesses);
      Guesses.clear();
      return SC.CheckAllGuesses || (!SC.CheckAllGuesses && RHSs.empty()); // Continue if no RHS
    }
    return true;
  };

  // add constant guess
  // TODO add a poison/undef guess
  if (!(OnlyInferI1 && SC.LHS->Width > 1))
    Guesses.push_back(IC.createSynthesisConstant(SC.LHS->Width, 1));

  // add nop guesses
  if (!OnlyInferI1 && !OnlyInferIN) {
    for (auto I : Cands) {
      if (I->Width == SC.LHS->Width)
        addGuess(I, SC.LHS->Width, SC.IC, LHSCost, Guesses, TooExpensive);
    }
  }

  if (DebugLevel > 1)
    llvm::errs() << "There are " << Guesses.size() << " guesses before enumeration\n";

  if (MaxNumInstructions > 0)
    getGuesses(Cands, SC.LHS->Width,
               LHSCost, SC.IC, nullptr, nullptr, TooExpensive, PruneCallback, Generate);

  if (DebugLevel > 1) {
    DataflowPruning.printStats(llvm::errs());
    llvm::errs() << "There are " << Guesses.size() << " total guesses\n";
    llvm::errs() << "(" << TooExpensive << " guesses were too expensive)\n";
  }

  if (!Guesses.empty() && !SkipSolver) {
    sortGuesses(Guesses);
    EC = verify(SC, RHSs, Guesses);
  }

  // RHSs count, before duplication
  if (DebugLevel > 3)
    llvm::errs() << "There are " << RHSs.size() << " RHSs before deduplication\n";

  std::set<Inst *> Dedup(RHSs.begin(), RHSs.end());
  RHSs.assign(Dedup.begin(), Dedup.end());

  // RHSs count, after duplication
  if (DebugLevel > 3)
    llvm::errs() << "There are " << RHSs.size() << " RHSs after deduplication\n";

  return EC;
}
