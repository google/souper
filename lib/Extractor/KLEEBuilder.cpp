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

#include "souper/Extractor/KLEEBuilder.h"

#include "klee/Expr.h"
#include "klee/util/ExprPPrinter.h"
#include "klee/util/ExprSMTLIBPrinter.h"
#include "klee/util/Ref.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CommandLine.h"
#include "souper/Inst/Inst.h"
#include "souper/Util/UniqueNameSet.h"
#include <functional>
#include <map>
#include <memory>
#include <sstream>
#include <unordered_map>

static llvm::cl::opt<bool> DumpKLEEExprs(
    "dump-klee-exprs",
    llvm::cl::desc("Dump KLEE expressions after SMTLIB queries"),
    llvm::cl::init(false));
static llvm::cl::opt<bool> ExploitUB(
    "souper-exploit-ub",
    llvm::cl::desc("Exploit undefined behavior (default=true)"),
    llvm::cl::init(true));

using namespace llvm;
using namespace klee;
using namespace souper;

namespace {

const unsigned MAX_PHI_DEPTH = 25;

typedef std::unordered_map<Inst *, std::vector<ref<Expr>>> UBPathInstMap;
typedef std::map<unsigned, ref<Expr>> BlockPCPredMap;

struct UBPath {
  std::map<Block *, unsigned> BlockConstraints;
  std::map<Inst *, bool> SelectBranches;
  std::vector<Inst *> Insts;
  std::vector<Inst *> UBInsts;
};

struct BlockPCPhiPath {
  std::map<Block *, unsigned> BlockConstraints;
  std::vector<Inst *> Phis;
  std::vector<ref<Expr>> PCs;
};

struct ExprBuilder {
  ExprBuilder(std::vector<std::unique_ptr<Array>> &Arrays,
              std::vector<Inst *> &ArrayVars)
      : Arrays(Arrays), ArrayVars(ArrayVars), IsForBlockPCUBInst(false) {}

  std::map<Block *, std::vector<ref<Expr>>> BlockPredMap;
  std::map<Inst *, ref<Expr>> ExprMap;
  std::map<Inst *, ref<Expr>> UBExprMap;
  std::map<Inst *, ref<Expr>> ZeroBitsMap;
  std::map<Inst *, ref<Expr>> OneBitsMap;
  std::map<Inst *, ref<Expr>> NonZeroBitsMap;
  std::map<Inst *, ref<Expr>> NonNegBitsMap;
  std::map<Inst *, ref<Expr>> PowerTwoBitsMap;
  std::map<Inst *, ref<Expr>> NegBitsMap;
  std::map<Inst *, ref<Expr>> SignBitsMap;
  std::map<Block *, BlockPCPredMap> BlockPCMap;
  std::vector<std::unique_ptr<Array>> &Arrays;
  std::vector<Inst *> &ArrayVars;
  std::vector<Inst *> UBPathInsts;
  UniqueNameSet ArrayNames;
  // Holding the precondition, i.e. blockpc, for the UBInst under process.
  ref<Expr> UBInstPrecondition;
  // Indicate if the UBInst relates to BlockPC
  bool IsForBlockPCUBInst;

  ref<Expr> makeSizedArrayRead(unsigned Width, StringRef Name, Inst *Origin);
  ref<Expr> addnswUB(Inst *I);
  ref<Expr> addnuwUB(Inst *I);
  ref<Expr> subnswUB(Inst *I);
  ref<Expr> subnuwUB(Inst *I);
  ref<Expr> mulnswUB(Inst *I);
  ref<Expr> mulnuwUB(Inst *I);
  ref<Expr> udivUB(Inst *I);
  ref<Expr> udivExactUB(Inst *I);
  ref<Expr> sdivUB(Inst *I);
  ref<Expr> sdivExactUB(Inst *I);
  ref<Expr> shiftUB(Inst *I);
  ref<Expr> shlnswUB(Inst *I);
  ref<Expr> shlnuwUB(Inst *I);
  ref<Expr> lshrExactUB(Inst *I);
  ref<Expr> ashrExactUB(Inst *I);
  ref<Expr> countOnes(ref<Expr> E);
  void recordUBInstruction(Inst *I, ref<Expr> E);
  ref<Expr> buildAssoc(std::function<ref<Expr>(ref<Expr>, ref<Expr>)> F,
                       llvm::ArrayRef<Inst *> Ops);
  ref<Expr> build(Inst *I);
  ref<Expr> get(Inst *I);
  ref<Expr> getInstMapping(const InstMapping &IM);
  ref<Expr> getZeroBitsMapping(Inst *I);
  ref<Expr> getOneBitsMapping(Inst *I);
  ref<Expr> getNonZeroBitsMapping(Inst *I);
  ref<Expr> getNonNegBitsMapping(Inst *I);
  ref<Expr> getPowerTwoBitsMapping(Inst *I);
  ref<Expr> getNegBitsMapping(Inst *I);
  ref<Expr> getSignBitsMapping(Inst *I);
  std::vector<ref<Expr >> getBlockPredicates(Inst *I);
  ref<Expr> getUBInstCondition();
  ref<Expr> getBlockPCs();
  void setBlockPCMap(const BlockPCs &BPCs);
  ref<Expr> createPathPred(std::map<Block *, unsigned> &BlockConstraints,
                           Inst* PathInst,
                           std::map<Inst *, bool> *SelectBranches);
  ref<Expr> createUBPathInstsPred(Inst *CurrentInst,
                           std::vector<Inst *> &UBPathInsts,
                           std::map<Block *, unsigned> &BlockConstraints,
                           std::map<Inst *, bool> *SelectBranches,
                           UBPathInstMap &CachedUBPathInsts);
  bool getUBPaths(Inst *I, UBPath *Current,
                  std::vector<std::unique_ptr<UBPath>> &Paths,
                  UBPathInstMap &CachedUBPathInsts, unsigned Depth);
  void getBlockPCPhiPaths(Inst *I, BlockPCPhiPath *Current,
                          std::vector<std::unique_ptr<BlockPCPhiPath>> &Paths,
                          UBPathInstMap &CachedPhis);
};

}

ref<Expr> ExprBuilder::makeSizedArrayRead(unsigned Width, StringRef Name,
                                          Inst *Origin) {
  std::string NameStr;
  if (Name.empty())
    NameStr = "arr";
  else if (Name[0] >= '0' && Name[0] <= '9')
    NameStr = ("a" + Name).str();
  else
    NameStr = Name;
  Arrays.emplace_back(
      new Array(ArrayNames.makeName(NameStr), 1, 0, 0, Expr::Int32, Width));
  ArrayVars.push_back(Origin);

  std::vector<unsigned> ZeroBits, OneBits;
  UpdateList UL(Arrays.back().get(), 0);
  ref<Expr> Var = ReadExpr::create(UL, klee::ConstantExpr::alloc(0, Expr::Int32));
  if (Origin && Origin->K == Inst::Var) {
    if (Origin->KnownZeros.getBoolValue() || Origin->KnownOnes.getBoolValue()) {
      ref<Expr> NotZeros = NotExpr::create(klee::ConstantExpr::alloc(Origin->KnownZeros));
      ref<Expr> VarOrNotZero = OrExpr::create(Var, NotZeros);
      ZeroBitsMap[Origin] = EqExpr::create(VarOrNotZero, NotZeros);
      ref<Expr> Ones = klee::ConstantExpr::alloc(Origin->KnownOnes);
      ref<Expr> VarAndOnes = AndExpr::create(Var, Ones);
      OneBitsMap[Origin] = EqExpr::create(VarAndOnes, Ones);
    }
    if (Origin->NonZero)
      NonZeroBitsMap[Origin] = NeExpr::create(Var, klee::ConstantExpr::create(0, Width));
    if (Origin->NonNegative)
      NonNegBitsMap[Origin] = SleExpr::create(klee::ConstantExpr::create(0, Width), Var);
    if (Origin->PowOfTwo) {
      ref<Expr> Zero = klee::ConstantExpr::create(0, Width);
      PowerTwoBitsMap[Origin] = AndExpr::create(NeExpr::create(Var, Zero),
                                                EqExpr::create(AndExpr::create(Var,
                                                SubExpr::create(Var, klee::ConstantExpr::create(1, Width))),
                                                Zero));
    }
    if (Origin->Negative)
      NegBitsMap[Origin] = SltExpr::create(Var, klee::ConstantExpr::create(0, Width));
    if (Origin->NumSignBits > 1) {
      ref<Expr> Res = AShrExpr::create(Var, klee::ConstantExpr::create(Width - Origin->NumSignBits, Width));
      ref<Expr> TestOnes = AShrExpr::create(ShlExpr::create(klee::ConstantExpr::create(1, Width),
                                                            klee::ConstantExpr::create(Width - 1, Width)),
                                            klee::ConstantExpr::create(Width - 1, Width));
      SignBitsMap[Origin] = OrExpr::create(EqExpr::create(Res, TestOnes),
                                           EqExpr::create(Res, klee::ConstantExpr::create(0, Width)));
    }
  }
  return Var;
}

ref<Expr> ExprBuilder::addnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Add = AddExpr::create(L, R);
   Expr::Width Width = L->getWidth();
   ref<Expr> LMSB = ExtractExpr::create(L, Width-1, Expr::Bool);
   ref<Expr> RMSB = ExtractExpr::create(R, Width-1, Expr::Bool);
   ref<Expr> AddMSB = ExtractExpr::create(Add, Width-1, Expr::Bool);
   return Expr::createImplies(EqExpr::create(LMSB, RMSB),
                              EqExpr::create(LMSB, AddMSB));
}

ref<Expr> ExprBuilder::addnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   Expr::Width Width = L->getWidth();
   ref<Expr> Lext = ZExtExpr::create(L, Width+1);
   ref<Expr> Rext = ZExtExpr::create(R, Width+1);
   ref<Expr> Add = AddExpr::create(Lext, Rext);
   ref<Expr> AddMSB = ExtractExpr::create(Add, Width, Expr::Bool);
   return Expr::createIsZero(AddMSB);
}

ref<Expr> ExprBuilder::subnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Sub = SubExpr::create(L, R);
   Expr::Width Width = L->getWidth();
   ref<Expr> LMSB = ExtractExpr::create(L, Width-1, Expr::Bool);
   ref<Expr> RMSB = ExtractExpr::create(R, Width-1, Expr::Bool);
   ref<Expr> SubMSB = ExtractExpr::create(Sub, Width-1, Expr::Bool);
   return Expr::createImplies(NeExpr::create(LMSB, RMSB),
                              EqExpr::create(LMSB, SubMSB));
}

ref<Expr> ExprBuilder::subnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   Expr::Width Width = L->getWidth();
   ref<Expr> Lext = ZExtExpr::create(L, Width+1);
   ref<Expr> Rext = ZExtExpr::create(R, Width+1);
   ref<Expr> Sub = SubExpr::create(Lext, Rext);
   ref<Expr> SubMSB = ExtractExpr::create(Sub, Width, Expr::Bool);
   return Expr::createIsZero(SubMSB);
}

ref<Expr> ExprBuilder::mulnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   // The computation below has to be performed on the operands of
   // multiplication instruction. The instruction using mulnswUB()
   // can be of different width, for instance in SMulO instruction
   // which is of 1-bit, but the operands width are to be used here.
   Expr::Width Width = get(Ops[0])->getWidth();
   ref<Expr> L = SExtExpr::create(get(Ops[0]), 2*Width);
   ref<Expr> R = SExtExpr::create(get(Ops[1]), 2*Width);
   ref<Expr> Mul = MulExpr::create(L, R);
   ref<Expr> LowerBits = ExtractExpr::create(Mul, 0, Width);
   ref<Expr> LowerBitsExt = SExtExpr::create(LowerBits, 2*Width);
   return EqExpr::create(Mul, LowerBitsExt);
}

ref<Expr> ExprBuilder::mulnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   Expr::Width Width = L->getWidth();
   ref<Expr> Lext = ZExtExpr::create(L, 2*Width);
   ref<Expr> Rext = ZExtExpr::create(R, 2*Width);
   ref<Expr> Mul = MulExpr::create(Lext, Rext);
   ref<Expr> HigherBits = ExtractExpr::create(Mul, Width, Width);
   return Expr::createIsZero(HigherBits);
}

ref<Expr> ExprBuilder::udivUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> R = get(Ops[1]);
   return NeExpr::create(R, klee::ConstantExpr::create(0, R->getWidth()));
}

ref<Expr> ExprBuilder::udivExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Udiv = UDivExpr::create(L, R);
   return EqExpr::create(L, MulExpr::create(R, Udiv));
}

ref<Expr> ExprBuilder::sdivUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> ShiftBy = klee::ConstantExpr::create(L->getWidth()-1,
                                                  L->getWidth());
   ref<Expr> IntMin = ShlExpr::create(klee::ConstantExpr::create(
                                      1, L->getWidth()), ShiftBy);
   ref<Expr> NegOne = AShrExpr::create(IntMin, ShiftBy);
   return AndExpr::create(NeExpr::create(R, klee::ConstantExpr::create(
                          0, R->getWidth())), OrExpr::create(
                          NeExpr::create(L, IntMin), NeExpr::create(R, NegOne)));
}

ref<Expr> ExprBuilder::sdivExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Sdiv = SDivExpr::create(L, R);
   return EqExpr::create(L, MulExpr::create(R, Sdiv));
}

ref<Expr> ExprBuilder::shiftUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Lwidth = klee::ConstantExpr::create(L->getWidth(), L->getWidth());
   return UltExpr::create(R, Lwidth);
}

ref<Expr> ExprBuilder::shlnswUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Result = ShlExpr::create(L, R);
   ref<Expr> RShift = AShrExpr::create(Result, R);
   return EqExpr::create(RShift, L);
}

ref<Expr> ExprBuilder::shlnuwUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Result = ShlExpr::create(L, R);
   ref<Expr> RShift = LShrExpr::create(Result, R);
   return EqExpr::create(RShift, L);
}

ref<Expr> ExprBuilder::lshrExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Result = LShrExpr::create(L, R);
   ref<Expr> LShift = ShlExpr::create(Result, R);
   return EqExpr::create(LShift, L);
}

ref<Expr> ExprBuilder::ashrExactUB(Inst *I) {
   const std::vector<Inst *> &Ops = I->orderedOps();
   ref<Expr> L = get(Ops[0]), R = get(Ops[1]);
   ref<Expr> Result = AShrExpr::create(L, R);
   ref<Expr> LShift = ShlExpr::create(Result, R);
   return EqExpr::create(LShift, L);
}

ref<Expr> ExprBuilder::buildAssoc(
    std::function<ref<Expr>(ref<Expr>, ref<Expr>)> F,
    llvm::ArrayRef<Inst *> Ops) {
  ref<Expr> E = get(Ops[0]);
  for (Inst *I : llvm::ArrayRef<Inst *>(Ops.data()+1, Ops.size()-1)) {
    E = F(E, get(I));
  }
  return E;
}

ref<Expr> ExprBuilder::countOnes(ref<Expr> L) {
   Expr::Width Width = L->getWidth();
   ref<Expr> Count =  klee::ConstantExpr::alloc(llvm::APInt(Width, 0));
   for (unsigned i=0; i<Width; i++) {
     ref<Expr> Bit = ExtractExpr::create(L, i, Expr::Bool);
     ref<Expr> BitExt = ZExtExpr::create(Bit, Width);
     Count = AddExpr::create(Count, BitExt);
   }
   return Count;
}

void ExprBuilder::recordUBInstruction(Inst *I, ref<Expr> E) {
  if (!IsForBlockPCUBInst) {
    UBExprMap[I] = E;
  }
  else if (!UBInstPrecondition.isNull()) {
    // The current UBInst comes from BlockPC. It's possible
    // that the precondition is missing at this point (e.g.,
    // the corresponding Phi is not part of the current
    // Souper IR because the Phi is not in the equivalence class
    // of the instruction.
    UBExprMap[I] = Expr::createImplies(UBInstPrecondition, E);
  }
}

ref<Expr> ExprBuilder::build(Inst *I) {
  const std::vector<Inst *> &Ops = I->orderedOps();
  switch (I->K) {
  case Inst::UntypedConst:
    assert(0 && "unexpected kind");
  case Inst::Const:
    return klee::ConstantExpr::alloc(I->Val);
  case Inst::Var:
    return makeSizedArrayRead(I->Width, I->Name, I);
  case Inst::Phi: {
    const auto &PredExpr = getBlockPredicates(I);
    ref<Expr> E = get(Ops[0]);
    // e.g. P2 ? (P1 ? Op1_Expr : Op2_Expr) : Op3_Expr
    for (unsigned J = 1; J < Ops.size(); ++J) {
      E = SelectExpr::create(PredExpr[J-1], E, get(Ops[J]));
    }
    UBPathInsts.push_back(I);
    return E;
  }
  case Inst::Add:
    return buildAssoc(AddExpr::create, Ops);
  case Inst::AddNSW: {
    ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, addnswUB(I));
    return Add;
  }
  case Inst::AddNUW: {
    ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, addnuwUB(I));
    return Add;
  }
  case Inst::AddNW: {
    ref<Expr> Add = AddExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, AndExpr::create(addnswUB(I), addnuwUB(I)));
    return Add;
  }
  case Inst::Sub:
    return SubExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::SubNSW: {
    ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, subnswUB(I));
    return Sub;
  }
  case Inst::SubNUW: {
    ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, subnuwUB(I));
    return Sub;
  }
  case Inst::SubNW: {
    ref<Expr> Sub = SubExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, AndExpr::create(subnswUB(I), subnuwUB(I)));
    return Sub;
  }
  case Inst::Mul:
    return buildAssoc(MulExpr::create, Ops);
  case Inst::MulNSW: {
    ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, mulnswUB(I));
    return Mul;
  }
  case Inst::MulNUW: {
    ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, mulnuwUB(I));
    return Mul;
  }
  case Inst::MulNW: {
    ref<Expr> Mul = MulExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, AndExpr::create(mulnswUB(I), mulnuwUB(I)));
    return Mul;
  }

  // We introduce these extra checks here because KLEE invokes llvm::APInt's
  // div functions , which crash upon divide-by-zero.
  case Inst::UDiv:
  case Inst::SDiv:
  case Inst::UDivExact:
  case Inst::SDivExact:
  case Inst::URem:
  case Inst::SRem: { // Fall-through
    // If the second oprand is 0, then it definitely causes UB.
    // There are quite a few cases where KLEE folds operations into zero,
    // e.g., "sext i16 0 to i32", "0 + 0", "2 - 2", etc.  In all cases,
    // we skip building the corresponding KLEE expressions and just return
    // a constant zero.
    ref<Expr> R = get(Ops[1]);
    if (R->isZero()) {
      recordUBInstruction(I, klee::ConstantExpr::create(0, 1));
      return klee::ConstantExpr::create(0, Ops[1]->Width);
    }

    switch (I->K) {
    default:
      break;

    case Inst::UDiv: {
      ref<Expr> Udiv = UDivExpr::create(get(Ops[0]), R);
      recordUBInstruction(I, udivUB(I));
      return Udiv;
    }
    case Inst::SDiv: {
      ref<Expr> Sdiv = SDivExpr::create(get(Ops[0]), R);
      recordUBInstruction(I, sdivUB(I));
      return Sdiv;
    }
    case Inst::UDivExact: {
      ref<Expr> Udiv = UDivExpr::create(get(Ops[0]), R);
      recordUBInstruction(I, AndExpr::create(udivUB(I), udivExactUB(I)));
      return Udiv;
    }
    case Inst::SDivExact: {
      ref<Expr> Sdiv = SDivExpr::create(get(Ops[0]), R);
      recordUBInstruction(I, AndExpr::create(sdivUB(I), sdivExactUB(I)));
      return Sdiv;
    }
    case Inst::URem: {
      ref<Expr> Urem = URemExpr::create(get(Ops[0]), R);
      recordUBInstruction(I, udivUB(I));
      return Urem;
    }
    case Inst::SRem: {
      ref<Expr> Srem = SRemExpr::create(get(Ops[0]), R);
      recordUBInstruction(I, sdivUB(I));
      return Srem;
    }
    llvm_unreachable("unknown kind");
  }
  }

  case Inst::And:
    return buildAssoc(AndExpr::create, Ops);
  case Inst::Or:
    return buildAssoc(OrExpr::create, Ops);
  case Inst::Xor:
    return buildAssoc(XorExpr::create, Ops);
  case Inst::Shl: {
    ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, shiftUB(I));
    return Result;
  }
  case Inst::ShlNSW: {
    ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, AndExpr::create(shiftUB(I), shlnswUB(I)));
    return Result;
  }
  case Inst::ShlNUW: {
    ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, AndExpr::create(shiftUB(I), shlnuwUB(I)));
    return Result;
  }
  case Inst::ShlNW: {
    ref<Expr> Result = ShlExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, AndExpr::create(shiftUB(I),
                                           AndExpr::create(shlnswUB(I),
                                                           shlnuwUB(I))));
    return Result;
  }
  case Inst::LShr: {
    ref<Expr> Result = LShrExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, shiftUB(I));
    return Result;
  }
  case Inst::LShrExact: {
    ref<Expr> Result = LShrExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, AndExpr::create(shiftUB(I), lshrExactUB(I)));
    return Result;
  }
  case Inst::AShr: {
    ref<Expr> Result = AShrExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, shiftUB(I));
    return Result;
  }
  case Inst::AShrExact: {
    ref<Expr> Result = AShrExpr::create(get(Ops[0]), get(Ops[1]));
    recordUBInstruction(I, AndExpr::create(shiftUB(I), ashrExactUB(I)));
    return Result;
  }
  case Inst::Select:
    UBPathInsts.push_back(I);
    return SelectExpr::create(get(Ops[0]), get(Ops[1]), get(Ops[2]));
  case Inst::ZExt:
    return ZExtExpr::create(get(Ops[0]), I->Width);
  case Inst::SExt:
    return SExtExpr::create(get(Ops[0]), I->Width);
  case Inst::Trunc:
    return ExtractExpr::create(get(Ops[0]), 0, I->Width);
  case Inst::Eq:
    return EqExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::Ne:
    return NeExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::Ult:
    return UltExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::Slt:
    return SltExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::Ule:
    return UleExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::Sle:
    return SleExpr::create(get(Ops[0]), get(Ops[1]));
  case Inst::CtPop:
    return countOnes(get(Ops[0]));
  case Inst::BSwap: {
    ref<Expr> L = get(Ops[0]);
    unsigned Width = L->getWidth();
    if (Width == 16) {
      return ConcatExpr::create(ExtractExpr::create(L, 0, 8),
                                ExtractExpr::create(L, 8, 8));
    }
    else if (Width == 32) {
      return ConcatExpr::create4(ExtractExpr::create(L, 0, 8),
                                 ExtractExpr::create(L, 8, 8),
                                 ExtractExpr::create(L, 16, 8),
                                 ExtractExpr::create(L, 24, 8));
    }
    else if (Width == 64) {
      return ConcatExpr::create8(ExtractExpr::create(L, 0, 8),
                                 ExtractExpr::create(L, 8, 8),
                                 ExtractExpr::create(L, 16, 8),
                                 ExtractExpr::create(L, 24, 8),
                                 ExtractExpr::create(L, 32, 8),
                                 ExtractExpr::create(L, 40, 8),
                                 ExtractExpr::create(L, 48, 8),
                                 ExtractExpr::create(L, 56, 8));
    }
    break;
  }
  case Inst::Cttz: {
    ref<Expr> L = get(Ops[0]);
    unsigned Width = L->getWidth();
    ref<Expr> Val = L;
    for (unsigned i=0, j=0; j<Width/2; i++) {
      j = 1<<i;
      Val = OrExpr::create(Val, ShlExpr::create(Val,
                           klee::ConstantExpr::create(j, Width)));
    }
    return SubExpr::create(klee::ConstantExpr::create(Width, Width),
                           countOnes(Val));
  }
  case Inst::Ctlz: {
    ref<Expr> L = get(Ops[0]);
    unsigned Width = L->getWidth();
    ref<Expr> Val = L;
    for (unsigned i=0, j=0; j<Width/2; i++) {
      j = 1<<i;
      Val = OrExpr::create(Val, LShrExpr::create(Val,
                           klee::ConstantExpr::create(j, Width)));
    }
    return SubExpr::create(klee::ConstantExpr::create(Width, Width),
                           countOnes(Val));
  }
  case Inst::SAddO:
    return XorExpr::create(addnswUB(I), klee::ConstantExpr::create(1, Expr::Bool));
  case Inst::UAddO:
    return XorExpr::create(addnuwUB(I), klee::ConstantExpr::create(1, Expr::Bool));
  case Inst::SSubO:
    return XorExpr::create(subnswUB(I), klee::ConstantExpr::create(1, Expr::Bool));
  case Inst::USubO:
    return XorExpr::create(subnuwUB(I), klee::ConstantExpr::create(1, Expr::Bool));
  case Inst::SMulO:
    return XorExpr::create(mulnswUB(I), klee::ConstantExpr::create(1, Expr::Bool));
  case Inst::UMulO:
    return XorExpr::create(mulnuwUB(I), klee::ConstantExpr::create(1, Expr::Bool));
  case Inst::ExtractValue: {
    unsigned Index = Ops[1]->Val.getZExtValue();
    return get(Ops[0]->Ops[Index]);
  }
  case Inst::SAddWithOverflow:
  case Inst::UAddWithOverflow:
  case Inst::SSubWithOverflow:
  case Inst::USubWithOverflow:
  case Inst::SMulWithOverflow:
  case Inst::UMulWithOverflow:
  default:
    break;
  }
  llvm_unreachable("unknown kind");
}

ref<Expr> ExprBuilder::get(Inst *I) {
  ref<Expr> &E = ExprMap[I];
  if (E.isNull()) {
    E = build(I);
    assert(E->getWidth() == I->Width);
  }
  return E;
}

ref<Expr> ExprBuilder::getInstMapping(const InstMapping &IM) {
  return EqExpr::create(get(IM.LHS), get(IM.RHS));
}

std::vector<ref<Expr>> ExprBuilder::getBlockPredicates(Inst *I) {
  assert(I->K == Inst::Phi && "not a phi inst");
  if (BlockPredMap.count(I->B))
    return BlockPredMap[I->B];
  std::vector<ref<Expr>> PredExpr;
  for (auto const &PredVar : I->B->PredVars)
    PredExpr.push_back(build(PredVar));
  BlockPredMap[I->B] = PredExpr;
  return PredExpr;
}

ref<Expr> ExprBuilder::createPathPred(
    std::map<Block *, unsigned> &BlockConstraints, Inst* PathInst,
    std::map<Inst *, bool> *SelectBranches) {

  ref<Expr> Pred = klee::ConstantExpr::alloc(1, 1);
  if (PathInst->K == Inst::Phi) {
    unsigned Num = BlockConstraints[PathInst->B];
    const auto &PredExpr = BlockPredMap[PathInst->B];
    // Sanity checks
    assert(PredExpr.size() && "there must be path predicates for the UBs");
    assert(PredExpr.size() == PathInst->Ops.size()-1 &&
           "phi predicate size mismatch");
    // Add the predicate(s)
    if (Num == 0)
      Pred = AndExpr::create(Pred, PredExpr[0]);
    else
      Pred = AndExpr::create(Pred, Expr::createIsZero(PredExpr[Num-1]));
    for (unsigned B = Num; B < PredExpr.size(); ++B)
      Pred = AndExpr::create(Pred, PredExpr[B]);
  }
  else if (PathInst->K == Inst::Select) {
    ref<Expr> SelectPred = get(PathInst->orderedOps()[0]);
    assert(SelectBranches && "NULL SelectBranches?");
    auto SI = SelectBranches->find(PathInst);
    // The current path doesn't have info about this select instruction.
    if (SI == SelectBranches->end()) {
      return Pred;
    }
    if (SI->second)
      Pred = AndExpr::create(Pred, SelectPred);
    else
      Pred = AndExpr::create(Pred, Expr::createIsZero(SelectPred));
  }
  else {
    assert(0 && "cannot reach here");
  }

  return Pred;
}

ref<Expr> ExprBuilder::createUBPathInstsPred(
    Inst *CurrentInst, std::vector<Inst *> &PathInsts,
    std::map<Block *, unsigned> &BlockConstraints,
    std::map<Inst *, bool> *SelectBranches, UBPathInstMap &CachedUBPathInsts) {
  ref<Expr> Pred = klee::ConstantExpr::alloc(1, 1);
  for (const auto &PathInst : PathInsts) {
    if (PathInst->Ops.size() == 1)
      continue;
    ref<Expr> InstPred =
      createPathPred(BlockConstraints, PathInst, SelectBranches);

    UBPathInstMap::iterator PI = CachedUBPathInsts.find(PathInst);
    if (PI == CachedUBPathInsts.end()) {
       // It's possible that we don't have a cached instruction yet,
       // e.g., the CurrentInst is a select operator.
       assert(CurrentInst->K == Inst::Select && "No cached Inst?");
       CachedUBPathInsts[PathInst] = {};
       PI = CachedUBPathInsts.find(PathInst);
    }
    if (PI->first != CurrentInst && PI->second.size() != 0) {
      // Use cached Expr along each path which has UB Insts,
      // and cache the expanded Expr for the current working Phi
      for (auto CE : PI->second) {
        InstPred = AndExpr::create(CE, InstPred);
        CachedUBPathInsts[CurrentInst].push_back(InstPred);
        Pred = AndExpr::create(Pred, InstPred);
      }
    }
    else {
      CachedUBPathInsts[CurrentInst].push_back(InstPred);
      Pred = AndExpr::create(Pred, InstPred);
    }
  }
  return Pred;
}

// Collect UB Inst condition for each Phi instruction.
// The basic algorithm is:
// (1) for a given phi instruction, we first collect all paths that start
//     from the phi. For each path, we also keep the the phi instructions
//     along the path and the UB instructions associated with these phi
//     instructions;
// (2) then for each path, we generate a corresponding KLEE expression
//     based on the facts that we collected in step (1), including:
//     * UB instructions;
//     * Phi predicates;
//
// With the algorithm, it is easy to get into the path explosion problem,
// i.e., the number of paths is increased exponentially. Under some
// circumstances, e.g., the number of phis is too large, we will suffer
// with large performance overhead. In some extreme cases, we will fail
// to process some file due to the large memory footprint, i.e., `newing'
// too many UBPaths. Two tricks are used to relief the penalty of the
// path explosion problem:
// (1) caching the KLEE expresions for each processed phi, where each
//     KLEE expression encodes the path that starts from one of the phi's
//     values. For example, when processing a sample souper IR below
//
//     %0 = block
//     %1 = block
//     %2:i32 = var
//     %3:i32 = shl 0:i32, %2
//     %4:i32 = var
//     %5:i32 = shl 0:i32, %4
//     %6 = var
//     %11:i32 = phi %1, %3, %5
//     %12:i32 = phi %0, %6, %11
//
//     we first encounter phi %11. The generated KLEE expression
//     for this phi encodes two paths, i.e., %3 and %5. We cache
//     these two into CachedUBPathInsts. Then we move to process phi %12.
//     At this point, rather than recursively re-contruct %11 (through
//     %12's value), we just re-used the cached path-expressions.
//     For each path-expression, we append it with %12's own predicate
//     and also cache them with %12 for future use. After finishing
//     %12, we will have three entries for phi %12.
// (2) The first trick increases the performance, but we still suffer
//     with large memory consumption, i.e., it's easy to cache too
//     many paths. The second trick is to reduce the memory footprint
//     by only caching "useful" path that has UB Insts. For example,
//     in the example in (1), for phi %12, we don't need to cache
//     the path starting from %6, because this path doesn't have any
//     UB Insts.
//
// These tricks basically relies on the dependency chain of instructions
// generated by souper. For example, if we say %12 depends on %11, then
// %12 would never appear earlier than %11.
ref<Expr> ExprBuilder::getUBInstCondition() {

  // A map from a Phi instruction to all of its KLEE expressions that
  // encode the path and UB Inst predicates.
  UBPathInstMap CachedUBPathInsts;
  std::set<Inst *> UsedUBInsts;
  ref<Expr> Result = klee::ConstantExpr::create(1, Expr::Bool);
  // For each Phi/Select instruction
  for (const auto &I : UBPathInsts) {
    if (CachedUBPathInsts.count(I) != 0)
      continue;
    // Recursively collect UB instructions
    // on the block constrained Phi and Select branches
    std::vector<std::unique_ptr<UBPath>> UBPaths;
    UBPath *Current = new UBPath;
    UBPaths.push_back(std::move(std::unique_ptr<UBPath>(Current)));
    if (!getUBPaths(I, Current, UBPaths, CachedUBPathInsts, 0))
      return ref<Expr>();
    CachedUBPathInsts[I] = {};
    // For each found path
    for (const auto &Path : UBPaths) {
      if (!Path->UBInsts.size())
        continue;
      // Aggregate collected UB constraints
      ref<Expr> Ante = klee::ConstantExpr::alloc(1, 1);
      for (const auto &I : Path->UBInsts) {
        auto Iter = UBExprMap.find(I);
        // It's possible that the instruction I is not in the map.
        // For example, it may come from a blockpc which doesn't
        // have any preconditions.
        if (Iter != UBExprMap.end())
          Ante = AndExpr::create(Ante, Iter->second);
        UsedUBInsts.insert(I);
      }
      // Create path predicate
      ref<Expr> Pred =
        createUBPathInstsPred(I, Path->Insts, Path->BlockConstraints,
                              &Path->SelectBranches, CachedUBPathInsts);
      // Add predicate->UB constraint
      Result = AndExpr::create(Result, Expr::createImplies(Pred, Ante));
    }
  }
  // Add the unconditional UB constraints at the top level
  for (const auto &Entry: UBExprMap)
    if (!UsedUBInsts.count(Entry.first))
      Result = AndExpr::create(Result, Entry.second);
  return Result;
}

bool ExprBuilder::getUBPaths(Inst *I, UBPath *Current,
                             std::vector<std::unique_ptr<UBPath>> &Paths,
                             UBPathInstMap &CachedUBPathInsts, unsigned Depth) {
  if (Depth > MAX_PHI_DEPTH)
    return false;

  switch (I->K) {
    default:
      break;

    case Inst::AddNSW:
    case Inst::AddNUW:
    case Inst::AddNW:
    case Inst::SubNSW:
    case Inst::SubNUW:
    case Inst::SubNW:
    case Inst::MulNSW:
    case Inst::MulNUW:
    case Inst::MulNW:
    case Inst::UDiv:
    case Inst::SDiv:
    case Inst::UDivExact:
    case Inst::SDivExact:
    case Inst::URem:
    case Inst::SRem:
    case Inst::Shl:
    case Inst::ShlNSW:
    case Inst::ShlNUW:
    case Inst::ShlNW:
    case Inst::LShr:
    case Inst::LShrExact:
    case Inst::AShr:
    case Inst::AShrExact:
      Current->UBInsts.push_back(I);
      break;
  }

  const std::vector<Inst *> &Ops = I->orderedOps();
  if (I->K == Inst::Phi) {
    // Early terminate because this phi has been processed.
    // We will use its cached predicates.
    if (CachedUBPathInsts.count(I))
      return true;
    Current->Insts.push_back(I);
    // Based on the dependency chain, looks like we would never
    // encounter this case.
    assert(!Current->BlockConstraints.count(I->B) &&
           "Basic block has been added into BlockConstraints!");
    std::vector<UBPath *> Tmp = { Current };
    // Create copies of the current path
    for (unsigned J = 1; J < Ops.size(); ++J) {
      UBPath *New = new UBPath;
      *New = *Current;
      New->BlockConstraints[I->B] = J;
      Paths.push_back(std::move(std::unique_ptr<UBPath>(New)));
      Tmp.push_back(New);
    }
    // Original path takes the first branch
    Current->BlockConstraints[I->B] = 0;
    // Continue recursively
    for (unsigned J = 0; J < Ops.size(); ++J) {
      if (!getUBPaths(Ops[J], Tmp[J], Paths, CachedUBPathInsts, Depth + 1))
        return false;
    }
  } else if (I->K == Inst::Select) {
    // Early terminate because this phi has been processed.
    // We will use its cached predicates.
    if (CachedUBPathInsts.count(I))
      return true;
    Current->Insts.push_back(I);
    // Current is the predicate operand branch
    std::vector<UBPath *> Tmp = { Current };
    // True branch
    UBPath *True = new UBPath;
    *True = *Current;
    True->SelectBranches[I] = true;
    Paths.push_back(std::move(std::unique_ptr<UBPath>(True)));
    Tmp.push_back(True);
    // False branch
    UBPath *False = new UBPath;
    *False = *Current;
    False->SelectBranches[I] = false;
    Paths.push_back(std::move(std::unique_ptr<UBPath>(False)));
    Tmp.push_back(False);
    // Continue recursively
    for (unsigned J = 0; J < Ops.size(); ++J) {
      if (!getUBPaths(Ops[J], Tmp[J], Paths, CachedUBPathInsts, Depth + 1))
        return false;
    }
  } else {
    for (unsigned J = 0; J < Ops.size(); ++J) {
      if (!getUBPaths(Ops[J], Current, Paths, CachedUBPathInsts, Depth + 1))
        return false;
    }
  }
  return true;
}

void ExprBuilder::setBlockPCMap(const BlockPCs &BPCs) {
  for (auto BPC : BPCs) {
    assert(BPC.B && "Block is NULL!");
    BlockPCPredMap &PCMap = BlockPCMap[BPC.B];
    auto I = PCMap.find(BPC.PredIdx);
    // Relying on a class-level flag may not be a nice solution,
    // but it seems hard to differentiate two cases:
    //   (1) UBInstExpr collected through blockpc, and;
    //   (2) UBInstExpr collected through pc/lhs/rhs
    // For the first case, UBInst(s) is conditional, i.e.,
    // they rely on the fact that blockpc(s) are true.
    if (I != PCMap.end()) {
      UBInstPrecondition = I->second;
    }
    IsForBlockPCUBInst = true;
    ref<Expr> PE = getInstMapping(BPC.PC);
    IsForBlockPCUBInst = false;
    UBInstPrecondition = nullptr;
    if (I == PCMap.end()) {
      PCMap[BPC.PredIdx] = PE;
    }
    else {
      PCMap[BPC.PredIdx] = AndExpr::create(I->second, PE);
    }
  }
}

// Similar to the way we collect UB constraints. We could combine it with 
// getUBInstCondition, because the workflow is quite similar. 
// However, mixing two parts (one for UB constraints, one for BlockPCs)
// may make the code less structured. If we see big performance overhead,
// we may consider to combine these two parts together. 
ref<Expr> ExprBuilder::getBlockPCs() {

  UBPathInstMap CachedPhis;
  ref<Expr> Result = klee::ConstantExpr::create(1, Expr::Bool);
  // For each Phi instruction
  for (const auto &I : UBPathInsts) {
    if (CachedPhis.count(I) != 0)
      continue;
    // Recursively collect BlockPCs
    std::vector<std::unique_ptr<BlockPCPhiPath>> BlockPCPhiPaths;
    BlockPCPhiPath *Current = new BlockPCPhiPath;
    BlockPCPhiPaths.push_back(
                        std::move(std::unique_ptr<BlockPCPhiPath>(Current)));
    getBlockPCPhiPaths(I, Current, BlockPCPhiPaths, CachedPhis);
    CachedPhis[I] = {};
    // For each found path
    for (const auto &Path : BlockPCPhiPaths) {
      if (!Path->PCs.size())
        continue;
      // Aggregate collected BlockPC constraints
      ref<Expr> Ante = klee::ConstantExpr::alloc(1, 1);
      for (const auto &PC : Path->PCs) {
        Ante = AndExpr::create(Ante, PC);
      }
      // Create path predicate
      ref<Expr> Pred =
        createUBPathInstsPred(I, Path->Phis, Path->BlockConstraints,
                              /*SelectBranches=*/nullptr, CachedPhis);
      // Add predicate->UB constraint
      Result = AndExpr::create(Result, Expr::createImplies(Pred, Ante));
    }
  }
  return Result;
}

void ExprBuilder::getBlockPCPhiPaths(
    Inst *I, BlockPCPhiPath *Current,
    std::vector<std::unique_ptr<BlockPCPhiPath>> &Paths,
    UBPathInstMap &CachedPhis) {

  const std::vector<Inst *> &Ops = I->orderedOps();
  if (I->K != Inst::Phi) {
    for (unsigned J = 0; J < Ops.size(); ++J)
      getBlockPCPhiPaths(Ops[J], Current, Paths, CachedPhis);
    return;
  }

  // Early terminate because this phi has been processed.
  // We will use its cached predicates.
  if (CachedPhis.count(I))
    return;
  Current->Phis.push_back(I);

  // Since we treat a select instruction as a phi instruction, it's
  // possible that I->B has been added already.
  if (Current->BlockConstraints.count(I->B))
    return;

  std::vector<BlockPCPhiPath *> Tmp = { Current };
  // Create copies of the current path
  for (unsigned J = 1; J < Ops.size(); ++J) {
    BlockPCPhiPath *New = new BlockPCPhiPath;
    *New = *Current;
    New->BlockConstraints[I->B] = J;
    Paths.push_back(std::move(std::unique_ptr<BlockPCPhiPath>(New)));
    Tmp.push_back(New);
  }
  // Original path takes the first branch
  Current->BlockConstraints[I->B] = 0;

  auto PCMap = BlockPCMap.find(I->B);
  if (PCMap != BlockPCMap.end()) {
    for (unsigned J = 0; J < Ops.size(); ++J) {
      auto P = PCMap->second.find(J);
      if (P != PCMap->second.end())
        Tmp[J]->PCs.push_back(P->second);
    }
  }
  // Continue recursively
  for (unsigned J = 0; J < Ops.size(); ++J)
    getBlockPCPhiPaths(Ops[J], Tmp[J], Paths, CachedPhis);
}

ref<Expr> ExprBuilder::getZeroBitsMapping(Inst *I) {
  return ZeroBitsMap[I];
}

ref<Expr> ExprBuilder::getOneBitsMapping(Inst *I) {
  return OneBitsMap[I];
}

ref<Expr> ExprBuilder::getNonZeroBitsMapping(Inst *I) {
  return NonZeroBitsMap[I];
}

ref<Expr> ExprBuilder::getNonNegBitsMapping(Inst *I) {
  return NonNegBitsMap[I];
}

ref<Expr> ExprBuilder::getNegBitsMapping(Inst *I) {
  return NegBitsMap[I];
}

ref<Expr> ExprBuilder::getPowerTwoBitsMapping(Inst *I) {
  return PowerTwoBitsMap[I];
}

ref<Expr> ExprBuilder::getSignBitsMapping(Inst *I) {
  return SignBitsMap[I];
}

// Return an expression which must be proven valid for the candidate to apply.
llvm::Optional<CandidateExpr> souper::GetCandidateExprForReplacement(
    const BlockPCs &BPCs, const std::vector<InstMapping> &PCs,
    InstMapping Mapping, bool Negate) {

  CandidateExpr CE;
  ExprBuilder EB(CE.Arrays, CE.ArrayVars);
  // Build LHS
  ref<Expr> LHS = EB.get(Mapping.LHS);
  ref<Expr> Ante = klee::ConstantExpr::alloc(1, 1);
  ref<Expr> DemandedBits = klee::ConstantExpr::alloc(Mapping.LHS->DemandedBits);
  if (!Mapping.LHS->DemandedBits.isAllOnesValue())
    LHS = AndExpr::create(LHS, DemandedBits);
  for (const auto I : CE.ArrayVars) {
    if (I) {
      if (I->KnownZeros.getBoolValue() || I->KnownOnes.getBoolValue()) {
        Ante = AndExpr::create(Ante, EB.getZeroBitsMapping(I));
        Ante = AndExpr::create(Ante, EB.getOneBitsMapping(I));
      }
      if (I->NonZero)
        Ante = AndExpr::create(Ante, EB.getNonZeroBitsMapping(I));
      if (I->NonNegative)
        Ante = AndExpr::create(Ante, EB.getNonNegBitsMapping(I));
      if (I->PowOfTwo)
        Ante = AndExpr::create(Ante, EB.getPowerTwoBitsMapping(I));
      if (I->Negative)
        Ante = AndExpr::create(Ante, EB.getNegBitsMapping(I));
      if (I->NumSignBits > 1)
        Ante = AndExpr::create(Ante, EB.getSignBitsMapping(I));
    }
  }
  // Build PCs
  for (const auto &PC : PCs) {
    Ante = AndExpr::create(Ante, EB.getInstMapping(PC));
  }
  // Build BPCs 
  if (BPCs.size()) {
    EB.setBlockPCMap(BPCs);
    Ante = AndExpr::create(Ante, EB.getBlockPCs());
  }
  // Get UB constraints of LHS and (B)PCs
  ref<Expr> LHSPCsUB = klee::ConstantExpr::create(1, Expr::Bool);
  if (ExploitUB) {
    LHSPCsUB = EB.getUBInstCondition();
    if (LHSPCsUB.isNull())
      return llvm::Optional<CandidateExpr>();
  }
  // Build RHS
  ref<Expr> RHS = EB.get(Mapping.RHS);
  if (!Mapping.LHS->DemandedBits.isAllOnesValue())
    RHS = AndExpr::create(RHS, DemandedBits);
  // Get all UB constraints (LHS && (B)PCs && RHS)
  ref<Expr> UB = klee::ConstantExpr::create(1, Expr::Bool);
  if (ExploitUB) {
    UB = EB.getUBInstCondition();
    if (UB.isNull())
      return llvm::Optional<CandidateExpr>();
  }

  ref<Expr> Cons;
  if (Negate) // (LHS != RHS)
    Cons = NeExpr::create(LHS, RHS);
  else        // (LHS == RHS)
    Cons = EqExpr::create(LHS, RHS);
  // Cons && UB
  if (Mapping.RHS->K != Inst::Const)
    Cons = AndExpr::create(Cons, UB);
  // (LHS UB && (B)PCs && (B)PCs UB)
  Ante = AndExpr::create(Ante, LHSPCsUB);
  // (LHS UB && (B)PCs && (B)PCs UB) => Cons && UB
  CE.E = Expr::createImplies(Ante, Cons);

  return llvm::Optional<CandidateExpr>(std::move(CE));
}

std::string souper::BuildQuery(const BlockPCs &BPCs,
                               const std::vector<InstMapping> &PCs,
                               InstMapping Mapping,
                               std::vector<Inst *> *ModelVars, bool Negate) {
  std::string SMTStr;
  llvm::raw_string_ostream SMTSS(SMTStr);
  ConstraintManager Manager;
  Optional<CandidateExpr> OptionalCE = GetCandidateExprForReplacement(BPCs,
      PCs, Mapping, Negate);
  if (!OptionalCE.hasValue())
    return std::string();
  CandidateExpr CE = std::move(OptionalCE.getValue());
  Query KQuery(Manager, CE.E);
  ExprSMTLIBPrinter Printer;
  Printer.setOutput(SMTSS);
  Printer.setQuery(KQuery);
  std::vector<const klee::Array *> Arrays;
  if (ModelVars) {
    for (unsigned I = 0; I != CE.ArrayVars.size(); ++I) {
      if (CE.ArrayVars[I]) {
        Arrays.push_back(CE.Arrays[I].get());
        ModelVars->push_back(CE.ArrayVars[I]);
      }
    }
    Printer.setArrayValuesToGet(Arrays);
  }
  Printer.generateOutput();

  if (DumpKLEEExprs) {
    SMTSS << "; KLEE expression:\n; ";
    std::unique_ptr<ExprPPrinter> PP(ExprPPrinter::create(SMTSS));
    PP->setForceNoLineBreaks(true);
    PP->scan(CE.E);
    PP->print(CE.E);
    SMTSS << '\n';
  }

  return SMTSS.str();
}
