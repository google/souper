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

#include "souper/Inst/Inst.h"

#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include <queue>
#include <set>

using namespace souper;

bool Inst::hasOrigin(llvm::Value *V) const {
  return std::find(Origins.begin(), Origins.end(), V) != Origins.end();
}

bool Inst::operator<(const Inst &Other) const {
  if (this == &Other)
    return false;

  if (K < Other.K)
    return true;
  if (K > Other.K)
    return false;

  if (Width < Other.Width)
    return true;
  if (Width > Other.Width)
    return false;

  switch (K) {
  case Const:
    return Val.ult(Other.Val);
  case UntypedConst: {
    llvm::APInt Val1 = Val, Val2 = Other.Val;
    if (Val1.getBitWidth() < Val2.getBitWidth())
      Val1 = Val1.sext(Val2.getBitWidth());
    else if (Val1.getBitWidth() > Val2.getBitWidth())
      Val2 = Val2.sext(Val1.getBitWidth());
    return Val1.slt(Val2);
  }
  case Var:
    return Number < Other.Number;
  case Phi:
    if (B->Number < Other.B->Number)
      return true;
    if (B->Number > Other.B->Number)
      return false;
  default:
    break;
  }

  if (Ops.size() < Other.Ops.size())
    return true;
  if (Ops.size() > Other.Ops.size())
    return false;

  const std::vector<Inst *> &OpsA = orderedOps();
  const std::vector<Inst *> &OpsB = Other.orderedOps();

  for (unsigned I = 0; I != OpsA.size(); ++I) {
    if (OpsA[I] == OpsB[I])
      continue;
    return (*OpsA[I] < *OpsB[I]);
  }

  if (HarvestKind == HarvestType::HarvestedFromDef &&
      Other.HarvestKind == HarvestType::HarvestedFromUse) {
    return false;
  }
  else if (HarvestKind == HarvestType::HarvestedFromUse &&
           Other.HarvestKind == HarvestType::HarvestedFromDef) {
    return true;
  }

  if (HarvestFrom != Other.HarvestFrom)
    return HarvestFrom < Other.HarvestFrom;
  return false;
}

const std::vector<Inst *> &Inst::orderedOps() const {
  if (!isCommutative(K))
    return Ops;

  if (OrderedOps.empty()) {
    OrderedOps = Ops;
    std::sort(OrderedOps.begin(), OrderedOps.end(), [](Inst *A, Inst *B) {
      return *A < *B;
    });
  }
  return OrderedOps;
}

std::string ReplacementContext::printInst(Inst *I, llvm::raw_ostream &Out,
                                          bool printNames) {
  return printInstImpl(I, Out, printNames, I);
}

std::string ReplacementContext::printInstImpl(Inst *I, llvm::raw_ostream &Out,
                                              bool printNames, Inst *OrigI) {

  std::string Str;
  llvm::raw_string_ostream SS(Str);

  auto PNI = InstNames.find(I);
  if (PNI != InstNames.end()) {
    SS << "%" << PNI->second;
    return SS.str();
  }

  std::string OpsStr;
  llvm::raw_string_ostream OpsSS(OpsStr);

  switch (I->K) {
  default:
    break;

  case Inst::Const:
    I->Val.print(SS, false);
    SS << ":i" << I->Val.getBitWidth();
    return SS.str();

  case Inst::UntypedConst:
    I->Val.print(SS, false);
    return SS.str();

  case Inst::Phi:
    std::string BlockName = printBlock(I->B, Out);
    OpsSS << " %" << BlockName << ",";
    break;
  }

  const std::vector<Inst *> &Ops = I->orderedOps();
  for (unsigned Idx = 0; Idx != Ops.size(); ++Idx) {
    if (Idx == 0)
      OpsSS << " ";
    else
      OpsSS << ", ";
    switch (I->K) {
      default:
        OpsSS << printInstImpl(Ops[Idx], Out, printNames, OrigI);
        break;
      case Inst::SAddWithOverflow:
      case Inst::UAddWithOverflow:
      case Inst::SSubWithOverflow:
      case Inst::USubWithOverflow:
      case Inst::SMulWithOverflow:
      case Inst::UMulWithOverflow:
        OpsSS << printInstImpl(I->Ops[1]->Ops[Idx], Out, printNames, OrigI);
        break;
    }
  }

  std::string InstName = std::to_string(InstNames.size() + BlockNames.size());
  assert(InstNames.find(I) == InstNames.end());
  assert(NameToBlock.find(InstName) == NameToBlock.end());
  setInst(InstName, I);

  // Skip the elements of overflow instruction tuple in souper IR
  switch (I->K) {
    case Inst::SAddO:
    case Inst::UAddO:
    case Inst::SSubO:
    case Inst::USubO:
    case Inst::SMulO:
    case Inst::UMulO:
      break;
    default: {
      Out << "%" << InstName << ":i" << I->Width << " = "
          << Inst::getKindName(I->K);
      if (I->K == Inst::Var) {
        if (I->KnownZeros.getBoolValue() || I->KnownOnes.getBoolValue())
          Out << " (knownBits=" << Inst::getKnownBitsString(I->KnownZeros, I->KnownOnes)
              << ")";
        if (I->NonNegative)
          Out << " (nonNegative)";
        if (I->Negative)
          Out << " (negative)";
        if (I->NonZero)
          Out << " (nonZero)";
        if (I->PowOfTwo)
          Out << " (powerOfTwo)";
        if (I->NumSignBits > 1)
          Out << " (signBits=" << I->NumSignBits << ")";
        if (!I->Range.isFullSet())
          Out << " (range=[" << I->Range.getLower()
              << "," << I->Range.getUpper() << "))";
      }
      Out << OpsSS.str();

      if (OrigI->DepsWithExternalUses.find(I) != OrigI->DepsWithExternalUses.end())
        Out << " (hasExternalUses)";

      if (printNames && !I->Name.empty())
        Out << " ; " << I->Name;
      Out << '\n';
      break;
    }
  }

  SS << "%" << InstName;
  return SS.str();
}

std::string ReplacementContext::printBlock(Block *B, llvm::raw_ostream &Out) {
  std::string Str;
  llvm::raw_string_ostream SS(Str);

  auto PNI = BlockNames.find(B);
  if (PNI != BlockNames.end()) {
    return PNI->second;
  }

  std::string BlockName = std::to_string(InstNames.size() + BlockNames.size());
  assert(BlockNames.find(B) == BlockNames.end());
  assert(NameToInst.find(BlockName) == NameToInst.end());
  assert(NameToBlock.find(BlockName) == NameToBlock.end());
  setBlock(BlockName, B);

  Out << '%' << BlockName << " = block " << B->Preds << "\n";
  return BlockName;
}

void ReplacementContext::clear() {
  InstNames.clear();
  BlockNames.clear();
  NameToInst.clear();
  NameToBlock.clear();
}

void ReplacementContext::printPCs(const std::vector<InstMapping> &PCs,
                                  llvm::raw_ostream &Out, bool printNames) {
  for (const auto &PC : PCs) {
    std::string SRef = printInst(PC.LHS, Out, printNames);
    std::string RRef = printInst(PC.RHS, Out, printNames);
    Out << "pc " << SRef << " " << RRef << '\n';
  }
}

void ReplacementContext::printBlockPCs(const BlockPCs &BPCs,
                                       llvm::raw_ostream &Out,
                                       bool printNames) {
  for (auto &BPC : BPCs) {
    assert(BPC.B && "NULL Block pointer!");
    std::string BlockName = printBlock(BPC.B, Out);
    std::string SRef = printInst(BPC.PC.LHS, Out, printNames);
    std::string RRef = printInst(BPC.PC.RHS, Out, printNames);
    Out << "blockpc %" << BlockName << " " << BPC.PredIdx << " ";
    Out << SRef << " " << RRef << '\n';
  }
}

bool ReplacementContext::empty() {
  return NameToInst.empty() && NameToBlock.empty();
}

Inst *ReplacementContext::getInst(llvm::StringRef Name) {
  auto InstIt = NameToInst.find(Name);
  return (InstIt == NameToInst.end()) ? 0 : InstIt->second;
}

void ReplacementContext::setInst(llvm::StringRef Name, Inst *I) {
  NameToInst[Name] = I;
  InstNames[I] = Name;
}

Block *ReplacementContext::getBlock(llvm::StringRef Name) {
  auto BlockIt = NameToBlock.find(Name);
  return (BlockIt == NameToBlock.end()) ? 0 : BlockIt->second;
}

void ReplacementContext::setBlock(llvm::StringRef Name, Block *B) {
  NameToBlock[Name] = B;
  BlockNames[B] = Name;
}

std::string Inst::getKnownBitsString(llvm::APInt Zero, llvm::APInt One) {
  std::string Str;
  for (int K = Zero.getBitWidth() - 1; K >= 0; --K) {
    if (Zero[K] && One[K])
      llvm_unreachable("KnownZero and KnownOnes bit can't be set to 1 together");
    if (Zero[K]) {
      Str.append("0");
    } else {
      if (One[K])
        Str.append("1");
      else
        Str.append("x");
    }
  }
  return Str;
}

std::string Inst::getDemandedBitsString(llvm::APInt DBVal) {
  std::string Str;
  for (int K = DBVal.getBitWidth() - 1; K >= 0; --K) {
    if (DBVal[K])
      Str.append("1");
    else
      Str.append("0");
  }
  return Str;
}

const char *Inst::getKindName(Kind K) {
  switch (K) {
  case Const:
    return "const";
  case UntypedConst:
    return "untypedconst";
  case Var:
    return "var";
  case Phi:
    return "phi";
  case Add:
    return "add";
  case AddNSW:
    return "addnsw";
  case AddNUW:
    return "addnuw";
  case AddNW:
    return "addnw";
  case Sub:
    return "sub";
  case SubNSW:
    return "subnsw";
  case SubNUW:
    return "subnuw";
  case SubNW:
    return "subnw";
  case Mul:
    return "mul";
  case MulNSW:
    return "mulnsw";
  case MulNUW:
    return "mulnuw";
  case MulNW:
    return "mulnw";
  case UDiv:
    return "udiv";
  case SDiv:
    return "sdiv";
  case UDivExact:
    return "udivexact";
  case SDivExact:
    return "sdivexact";
  case URem:
    return "urem";
  case SRem:
    return "srem";
  case And:
    return "and";
  case Or:
    return "or";
  case Xor:
    return "xor";
  case Shl:
    return "shl";
  case ShlNSW:
    return "shlnsw";
  case ShlNUW:
    return "shlnuw";
  case ShlNW:
    return "shlnw";
  case LShr:
    return "lshr";
  case LShrExact:
    return "lshrexact";
  case AShr:
    return "ashr";
  case AShrExact:
    return "ashrexact";
  case Select:
    return "select";
  case ZExt:
    return "zext";
  case SExt:
    return "sext";
  case Trunc:
    return "trunc";
  case Eq:
    return "eq";
  case Ne:
    return "ne";
  case Ult:
    return "ult";
  case Slt:
    return "slt";
  case Ule:
    return "ule";
  case Sle:
    return "sle";
  case CtPop:
    return "ctpop";
  case BSwap:
    return "bswap";
  case BitReverse:
    return "bitreverse";
  case Cttz:
    return "cttz";
  case Ctlz:
    return "ctlz";
  case FShl:
    return "fshl";
  case FShr:
    return "fshr";
  case ExtractValue:
    return "extractvalue";
  case SAddWithOverflow:
    return "sadd.with.overflow";
  case UAddWithOverflow:
    return "uadd.with.overflow";
  case SSubWithOverflow:
    return "ssub.with.overflow";
  case USubWithOverflow:
    return "usub.with.overflow";
  case SMulWithOverflow:
    return "smul.with.overflow";
  case UMulWithOverflow:
    return "umul.with.overflow";
  case SAddSat:
    return "sadd.sat";
  case UAddSat:
    return "uadd.sat";
  case SSubSat:
    return "ssub.sat";
  case USubSat:
    return "usub.sat";
  case ReservedConst:
    return "reservedconst";
  case ReservedInst:
    return "reservedinst";
  case Hole:
    return "hole";
  case SAddO:
  case UAddO:
  case SSubO:
  case USubO:
  case SMulO:
  case UMulO:
  default:
    llvm_unreachable("all cases covered");
  }
}

Inst::Kind Inst::getKind(std::string Name) {
  return llvm::StringSwitch<Inst::Kind>(Name)
                   .Case("var", Inst::Var)
                   .Case("phi", Inst::Phi)
                   .Case("add", Inst::Add)
                   .Case("addnsw", Inst::AddNSW)
                   .Case("addnuw", Inst::AddNUW)
                   .Case("addnw", Inst::AddNW)
                   .Case("sub", Inst::Sub)
                   .Case("subnsw", Inst::SubNSW)
                   .Case("subnuw", Inst::SubNUW)
                   .Case("subnw", Inst::SubNW)
                   .Case("mul", Inst::Mul)
                   .Case("mulnsw", Inst::MulNSW)
                   .Case("mulnuw", Inst::MulNUW)
                   .Case("mulnw", Inst::MulNW)
                   .Case("udiv", Inst::UDiv)
                   .Case("sdiv", Inst::SDiv)
                   .Case("udivexact", Inst::UDivExact)
                   .Case("sdivexact", Inst::SDivExact)
                   .Case("urem", Inst::URem)
                   .Case("srem", Inst::SRem)
                   .Case("and", Inst::And)
                   .Case("or", Inst::Or)
                   .Case("xor", Inst::Xor)
                   .Case("shl", Inst::Shl)
                   .Case("shlnsw", Inst::ShlNSW)
                   .Case("shlnuw", Inst::ShlNUW)
                   .Case("shlnw", Inst::ShlNW)
                   .Case("lshr", Inst::LShr)
                   .Case("lshrexact", Inst::LShrExact)
                   .Case("ashr", Inst::AShr)
                   .Case("ashrexact", Inst::AShrExact)
                   .Case("select", Inst::Select)
                   .Case("zext", Inst::ZExt)
                   .Case("sext", Inst::SExt)
                   .Case("trunc", Inst::Trunc)
                   .Case("eq", Inst::Eq)
                   .Case("ne", Inst::Ne)
                   .Case("ult", Inst::Ult)
                   .Case("slt", Inst::Slt)
                   .Case("ule", Inst::Ule)
                   .Case("sle", Inst::Sle)
                   .Case("ctpop", Inst::CtPop)
                   .Case("bswap", Inst::BSwap)
                   .Case("bitreverse", Inst::BitReverse)
                   .Case("cttz", Inst::Cttz)
                   .Case("ctlz", Inst::Ctlz)
                   .Case("fshl", Inst::FShl)
                   .Case("fshr", Inst::FShr)
                   .Case("sadd.with.overflow", Inst::SAddWithOverflow)
                   .Case("uadd.with.overflow", Inst::UAddWithOverflow)
                   .Case("ssub.with.overflow", Inst::SSubWithOverflow)
                   .Case("usub.with.overflow", Inst::USubWithOverflow)
                   .Case("smul.with.overflow", Inst::SMulWithOverflow)
                   .Case("umul.with.overflow", Inst::UMulWithOverflow)
                   .Case("sadd.sat", Inst::SAddSat)
                   .Case("uadd.sat", Inst::UAddSat)
                   .Case("ssub.sat", Inst::SSubSat)
                   .Case("usub.sat", Inst::USubSat)
                   .Case("extractvalue", Inst::ExtractValue)
                   .Case("reservedinst", Inst::ReservedInst)
                   .Case("hole", Inst::Hole)
                   .Case("reservedconst", Inst::ReservedConst)
                   .Default(Inst::None);
}

void Inst::Profile(llvm::FoldingSetNodeID &ID) const {
  ID.AddInteger(K);
  ID.AddInteger(Width);

  switch (K) {
  case Const:
  case UntypedConst:
    Val.Profile(ID);
    return;
  case Var:
    assert(0 && "Var instructions should not be in FoldingSet");
  case Phi:
    ID.AddPointer(B);
    break;
  default:
    if (!DemandedBits.isAllOnesValue())
      ID.Add(DemandedBits);
    if (HarvestKind == HarvestType::HarvestedFromUse) {
      ID.Add(HarvestFrom);
    }
    break;
  }

  for (auto Op : Ops)
    ID.AddPointer(Op);
}

Inst *InstContext::getConst(const llvm::APInt &Val) {
  llvm::FoldingSetNodeID ID;
  ID.AddInteger(Inst::Const);
  ID.AddInteger(Val.getBitWidth());
  Val.Profile(ID);

  void *IP = 0;
  if (Inst *I = InstSet.FindNodeOrInsertPos(ID, IP))
    return I;

  auto N = new Inst;
  Insts.emplace_back(N);
  N->K = Inst::Const;
  N->Width = Val.getBitWidth();
  N->Val = Val;
  InstSet.InsertNode(N, IP);
  return N;
}

Inst *InstContext::getUntypedConst(const llvm::APInt &Val) {
  llvm::FoldingSetNodeID ID;
  ID.AddInteger(Inst::UntypedConst);
  ID.AddInteger(0);
  Val.Profile(ID);

  void *IP = 0;
  if (Inst *I = InstSet.FindNodeOrInsertPos(ID, IP))
    return I;

  auto N = new Inst;
  Insts.emplace_back(N);
  N->K = Inst::UntypedConst;
  N->Width = 0;
  N->Val = Val;
  InstSet.InsertNode(N, IP);
  return N;
}

Inst *InstContext::getReservedConst() {
  auto N = new Inst;
  Insts.emplace_back(N);
  N->K = Inst::ReservedConst;
  N->SynthesisConstID = ++ReservedConstCounter;
  N->Width = 0;
  return N;
}

Inst *InstContext::getReservedInst() {
  auto N = new Inst;
  Insts.emplace_back(N);
  N->K = Inst::ReservedInst;
  N->Width = 0;
  return N;
}

Inst *InstContext::createHole(unsigned Width) {
  auto N = new Inst;
  Insts.emplace_back(N);
  N->K = Inst::Hole;
  N->Width = Width;
  return N;
}

Inst *InstContext::createVar(unsigned Width, llvm::StringRef Name,
                             llvm::ConstantRange Range,
                             llvm::APInt Zero, llvm::APInt One, bool NonZero,
                             bool NonNegative, bool PowOfTwo, bool Negative,
                             unsigned NumSignBits, unsigned SynthesisConstID) {
  // Create a new vector of Insts if Width is not found in VarInstsByWidth
  auto &InstList = VarInstsByWidth[Width];
  unsigned Number = InstList.size();
  auto I = new Inst;
  InstList.emplace_back(I);
  assert(Range.getBitWidth() == Width && Zero.getBitWidth() == Width && One.getBitWidth() == Width);

  I->K = Inst::Var;
  I->Number = Number;
  I->Width = Width;
  I->Name = Name;
  I->Range = Range;
  I->KnownZeros = Zero;
  I->KnownOnes = One;
  I->NonZero = NonZero;
  I->NonNegative = NonNegative;
  I->PowOfTwo = PowOfTwo;
  I->Negative = Negative;
  I->NumSignBits = NumSignBits;
  I->SynthesisConstID = SynthesisConstID;
  return I;
}

Inst *InstContext::createVar(unsigned Width, llvm::StringRef Name) {
  return createVar(Width, Name, /*Range=*/llvm::ConstantRange(Width, /*isFullSet=*/ true),
                    /*KnownZero=*/ llvm::APInt(Width, 0), /*KnownOne=*/ llvm::APInt(Width, 0),
                    /*NonZero=*/ false, /*NonNegative=*/ false, /*PowerOfTwo=*/ false,
                    /*Negative=*/ false, /*SignBits=*/ 1, /*SynthesisConstID=*/0);
}

Inst *InstContext::createSynthesisConstant(unsigned Width, unsigned SynthesisConstID) {
  return createVar(Width,  ReservedConstPrefix + std::to_string(SynthesisConstID),
                   /*Range=*/llvm::ConstantRange(Width, /*isFullSet=*/ true),
                   /*KnownZero=*/ llvm::APInt(Width, 0), /*KnownOne=*/ llvm::APInt(Width, 0),
                   /*NonZero=*/ false, /*NonNegative=*/ false, /*PowerOfTwo=*/ false,
                   /*Negative=*/ false, /*SignBits=*/ 1, /*SynthesisConstID=*/SynthesisConstID);
}


Block *InstContext::createBlock(unsigned Preds) {
  auto &BlockList = BlocksByPreds[Preds];
  unsigned Number = BlockList.size();
  auto B = new Block;
  BlockList.emplace_back(B);

  B->Number = Number;
  B->Preds = Preds;
  for (unsigned J = 0; J < Preds-1; ++J)
    B->PredVars.push_back(createVar(1, "blockpred"));
  return B;
}

Inst *InstContext::getPhi(Block *B, const std::vector<Inst *> &Ops, llvm::APInt DemandedBits) {
  llvm::FoldingSetNodeID ID;
  ID.AddInteger(Inst::Phi);
  ID.AddInteger(Ops[0]->Width);
  ID.AddPointer(B);
  for (auto O : Ops)
    ID.AddPointer(O);
  if (!DemandedBits.isAllOnesValue())
    ID.Add(DemandedBits);

  void *IP = 0;
  if (Inst *I = InstSet.FindNodeOrInsertPos(ID, IP))
    return I;

  auto N = new Inst;
  Insts.emplace_back(N);
  N->K = Inst::Phi;
  N->Width = Ops[0]->Width;
  N->B = B;
  N->Ops = Ops;
  N->DemandedBits = DemandedBits;
  InstSet.InsertNode(N, IP);
  return N;
}

Inst *InstContext::getPhi(Block *B, const std::vector<Inst *> &Ops) {
  llvm::APInt DemandedBits = llvm::APInt::getAllOnesValue(Ops[0]->Width);
  return getPhi(B, Ops, DemandedBits);
}


Inst *InstContext::getInst(Inst::Kind K, unsigned Width,
                           const std::vector<Inst *> &Ops,
                           llvm::APInt DemandedBits, bool Available) {
  std::vector<Inst *> OrderedOps;

  const std::vector<Inst *> *InstOps;
  if (Inst::isCommutative(K)) {
    OrderedOps = Ops;
    std::sort(OrderedOps.begin(), OrderedOps.end());
    InstOps = &OrderedOps;
  } else {
    InstOps = &Ops;
  }

  llvm::FoldingSetNodeID ID;
  ID.AddInteger(K);
  ID.AddInteger(Width);
  for (auto O : *InstOps)
    ID.AddPointer(O);
  if (!DemandedBits.isAllOnesValue())
    ID.Add(DemandedBits);

  void *IP = 0;
  if (Inst *I = InstSet.FindNodeOrInsertPos(ID, IP))
    return I;

  auto N = new Inst;
  Insts.emplace_back(N);
  N->K = K;
  N->Width = Width;
  N->Ops = *InstOps;
  N->DemandedBits = DemandedBits;
  N->Available = Available;
  N->HarvestKind = HarvestType::HarvestedFromDef;
  N->HarvestFrom = nullptr;
  InstSet.InsertNode(N, IP);
  return N;
}

Inst *InstContext::getInst(Inst::Kind K, unsigned Width,
                           const std::vector<Inst *> &Ops,
                           bool Available) {
  llvm::APInt DemandedBits = llvm::APInt::getAllOnesValue(Width);
  return getInst(K, Width, Ops, DemandedBits, Available);
}

bool Inst::isCommutative(Inst::Kind K) {
  switch (K) {
  case Add:
  case AddNSW:
  case AddNUW:
  case AddNW:
  case SAddSat:
  case UAddSat:
  case Mul:
  case MulNSW:
  case MulNUW:
  case MulNW:
  case And:
  case Or:
  case Xor:
  case Eq:
  case Ne:
    return true;
  default:
    return false;
  }
}

bool Inst::isCmp(Inst::Kind K) {
  return K == Inst::Eq || K == Inst::Ne || K == Inst::Ult ||
    K == Inst::Slt || K == Inst::Ule || K == Inst::Sle;
}

bool Inst::isShift(Inst::Kind K) {
  return K == Inst::Shl || K == Inst::AShr || K == Inst::LShr;
}

int Inst::getCost(Inst::Kind K) {
  switch (K) {
    case Var:
    case Const:
    case Phi:
      return 0;
    case BSwap:
    case CtPop:
    case Cttz:
    case Ctlz:
    case SDiv:
    case UDiv:
    case SRem:
    case URem:
      return 5;
    case FShl:
    case FShr:
    case SAddSat:
    case UAddSat:
    case SSubSat:
    case USubSat:
      return 3;
    case Select:
      return 3;
    default:
      return 1;
  }
}

static int costHelper(Inst *I, Inst *Root, std::set<Inst *> &Visited,
                      bool IgnoreDepsWithExternalUses) {
  if (!Visited.insert(I).second)
    return 0;
  if (IgnoreDepsWithExternalUses && I != Root &&
      Root->DepsWithExternalUses.find(I) != Root->DepsWithExternalUses.end()) {
    return 0;
  }
  int Cost = Inst::getCost(I->K);
  for (auto Op : I->Ops)
    Cost += costHelper(Op, Root, Visited, IgnoreDepsWithExternalUses);
  return Cost;
}

int souper::cost(Inst *I, bool IgnoreDepsWithExternalUses) {
  std::set<Inst *> Visited;
  return costHelper(I, I, Visited, IgnoreDepsWithExternalUses);
}


static int countHelper(Inst *I, std::set<Inst *> &Visited) {
  if (!Visited.insert(I).second)
    return 0;

  int Count;

  if (I->K == Inst::Var || I->K == Inst::Const || I->K == Inst::Hole)
    Count = 0;
  else
    Count = 1;

  for (auto Op : I->Ops)
    Count += countHelper(Op, Visited);
  return Count;
}

int souper::instCount(Inst *I) {
  std::set<Inst *> Visited;
  return countHelper(I, Visited);
}

int souper::benefit(Inst *LHS, Inst *RHS) {
  return cost(LHS, /*IgnoreDepsWithExternalUses=*/true) - cost(RHS);
}

void souper::PrintReplacement(llvm::raw_ostream &Out,
                              const BlockPCs &BPCs,
                              const std::vector<InstMapping> &PCs,
                              InstMapping Mapping, bool printNames) {
  assert(Mapping.LHS);
  assert(Mapping.RHS);

  ReplacementContext Context;
  Context.printPCs(PCs, Out, printNames);
  Context.printBlockPCs(BPCs, Out, printNames);
  std::string SRef = Context.printInst(Mapping.LHS, Out, printNames);
  std::string RRef = Context.printInst(Mapping.RHS, Out, printNames);
  Out << "cand " << SRef << " " << RRef;
  if (!Mapping.LHS->DemandedBits.isAllOnesValue()) {
    Out<< " (" << "demandedBits="
       << Inst::getDemandedBitsString(Mapping.LHS->DemandedBits)
       << ")";
  }
  if (Mapping.LHS->HarvestKind == HarvestType::HarvestedFromUse) {
    Out << " (harvestedFromUse)";
  }
  Out << "\n";
}

std::string souper::GetReplacementString(const BlockPCs &BPCs,
                                         const std::vector<InstMapping> &PCs,
                                         InstMapping Mapping, bool printNames) {
  std::string Str;
  llvm::raw_string_ostream SS(Str);
  PrintReplacement(SS, BPCs, PCs, Mapping, printNames);
  return SS.str();
}

void souper::PrintReplacementLHS(llvm::raw_ostream &Out,
                                 const BlockPCs &BPCs,
                                 const std::vector<InstMapping> &PCs,
                                 Inst *LHS, ReplacementContext &Context,
                                 bool printNames) {
  assert(LHS);
  assert(Context.empty());

  Context.printPCs(PCs, Out, printNames);
  Context.printBlockPCs(BPCs, Out, printNames);
  std::string SRef = Context.printInst(LHS, Out, printNames);

  Out << "infer " << SRef;
  if (!LHS->DemandedBits.isAllOnesValue()) {
    Out<< " (" << "demandedBits="
       << Inst::getDemandedBitsString(LHS->DemandedBits)
       << ")";
  }
  if (LHS->HarvestKind == HarvestType::HarvestedFromUse) {
    Out << " (harvestedFromUse)";
  }
  Out << "\n";
}

std::string souper::GetReplacementLHSString(const BlockPCs &BPCs,
    const std::vector<InstMapping> &PCs,
    Inst *LHS, ReplacementContext &Context, bool printNames) {
  std::string Str;
  llvm::raw_string_ostream SS(Str);
  PrintReplacementLHS(SS, BPCs, PCs, LHS, Context);
  return SS.str();
}

void souper::PrintReplacementRHS(llvm::raw_ostream &Out, Inst *RHS,
                                 ReplacementContext &Context, bool printNames) {
  std::string SRef = Context.printInst(RHS, Out, printNames);
  Out << "result " << SRef << '\n';
}

std::string souper::GetReplacementRHSString(Inst *RHS,
                                            ReplacementContext &Context,
                                            bool printNames) {
  std::string Str;
  llvm::raw_string_ostream SS(Str);
  PrintReplacementRHS(SS, RHS, Context, printNames);
  return SS.str();
}

void souper::findCands(Inst *Root, std::vector<Inst *> &Guesses,
               bool WidthMustMatch, bool FilterVars, int Max) {
  // breadth-first search
  std::set<Inst *> Visited;
  std::queue<std::tuple<Inst *,int>> Q;
  Q.push(std::make_tuple(Root, 0));
  while (!Q.empty()) {
    Inst *I;
    int Benefit;
    std::tie(I, Benefit) = Q.front();
    Q.pop();
    ++Benefit;
    if (Visited.insert(I).second) {
      for (auto Op : I->Ops)
        Q.push(std::make_tuple(Op, Benefit));
      if (Benefit > 1 && I->Available && I->K != Inst::Const
          && I->K != Inst::UntypedConst) {
        if (WidthMustMatch && I->Width != Root->Width)
          continue;
        if (FilterVars && I->K == Inst::Var)
          continue;
        if (I->K == Inst::SAddWithOverflow || I->K == Inst::UAddWithOverflow ||
            I->K == Inst::SSubWithOverflow || I->K == Inst::USubWithOverflow ||
            I->K == Inst::SMulWithOverflow || I->K == Inst::UMulWithOverflow)
          continue;
        Guesses.emplace_back(I);
        if (Guesses.size() >= Max)
          return;
      }
    }
  }
}

/* TODO call findCands instead */
void souper::findVars(Inst *Root, std::vector<Inst *> &Vars) {
  // breadth-first search
  std::set<Inst *> Visited;
  std::queue<Inst *> Q;
  Q.push(Root);
  while (!Q.empty()) {
    Inst *I = Q.front();
    Q.pop();
    if (!Visited.insert(I).second)
      continue;
    if (I->K == Inst::Var && I->SynthesisConstID == 0)
      Vars.push_back(I);
    for (auto Op : I->Ops)
      Q.push(Op);
  }
}

void hasConstantHelper(Inst *I, std::set<Inst *> &Visited,
                       std::set<Inst *> &ConstSet) {
  if (I->K == Inst::Var && I->SynthesisConstID != 0) {
    ConstSet.insert(I);
  } else {
    if (Visited.insert(I).second)
      for (auto Op : I->Ops)
        hasConstantHelper(Op, Visited, ConstSet);
  }
}

// TODO do this a more efficient way
void souper::getConstants(Inst *I, std::set<Inst *> &ConstSet) {
  std::set<Inst *> Visited;
  hasConstantHelper(I, Visited, ConstSet);
}



// TODO: Convert to a more generic getGivenInst similar to hasGivenInst below
void souper::getHoles(Inst *Root, std::vector<Inst *> &Holes) {
  // breadth-first search
  std::set<Inst *> Visited;
  std::queue<Inst *> Q;
  Q.push(Root);
  while (!Q.empty()) {
    Inst *I = Q.front();
    Q.pop();
    if (!Visited.insert(I).second)
      continue;
    if (I->K == Inst::Hole) {
      assert(I->Width > 0);
      Holes.push_back(I);
    }
    for (auto Op : I->Ops)
      Q.push(Op);
  }
}

bool souper::hasGivenInst(Inst *Root, std::function<bool(Inst*)> InstTester) {
  // breadth-first search
  std::set<Inst *> Visited;
  std::queue<Inst *> Q;
  Q.push(Root);
  while (!Q.empty()) {
    Inst *I = Q.front();
    Q.pop();
    if (InstTester(I))
      return true;
    if (!Visited.insert(I).second)
      continue;
    for (auto Op : I->Ops)
      Q.push(Op);
  }
  return false;
}

Inst *souper::getInstCopy(Inst *I, InstContext &IC,
                          std::map<Inst *, Inst *> &InstCache,
                          std::map<Block *, Block *> &BlockCache,
                          std::map<Inst *, llvm::APInt> *ConstMap,
                          bool CloneVars) {

  if (InstCache.count(I))
    return InstCache.at(I);

  std::vector<Inst *> Ops;
  for (auto const &Op : I->Ops)
    Ops.push_back(getInstCopy(Op, IC, InstCache, BlockCache, ConstMap, CloneVars));

  Inst *Copy = 0;
  if (I->K == Inst::Var) {
    if (ConstMap) {
      auto it = ConstMap->find(I);
      if (it != ConstMap->end()) {
	{
	  llvm::APInt x = it->second;
          /*
	  llvm::outs() << "found a var to replace with a constant width " <<
	    x.getBitWidth() << " and value " << x << "\n";
          */
	}
	Copy = IC.getConst(it->second);
      }
    }
    if (!Copy) {
      if (CloneVars && I->SynthesisConstID == 0)
        Copy = IC.createVar(I->Width, I->Name, I->Range, I->KnownZeros,
                            I->KnownOnes, I->NonZero, I->NonNegative,
                            I->PowOfTwo, I->Negative, I->NumSignBits,
                            I->SynthesisConstID);
      else {
        Copy = I;
      }
    }
  } else if (I->K == Inst::Phi) {
    if (!BlockCache.count(I->B)) {
      auto BlockCopy = IC.createBlock(I->B->Preds);
      BlockCache[I->B] = BlockCopy;
      Copy = IC.getPhi(BlockCopy, Ops, I->DemandedBits);
    } else {
      Copy = IC.getPhi(BlockCache.at(I->B), Ops, I->DemandedBits);
    }
  } else if (I->K == Inst::Const || I->K == Inst::UntypedConst) {
    return I;
  } else {
    Copy = IC.getInst(I->K, I->Width, Ops, I->DemandedBits, I->Available);
  }
  assert(Copy);
  InstCache[I] = Copy;
  return Copy;
}

Inst *souper::instJoin(Inst *I, Inst *EmptyInst, Inst *NewInst,
                       InstContext &IC) {
  std::vector<Inst *> Ops;

  for (auto const &Op : I->Ops) {
    auto NewOp = instJoin(Op, EmptyInst, NewInst, IC);
    Ops.push_back(NewOp);
  }

  Inst *Copy = 0;
  if (I == EmptyInst) {
    Copy = NewInst;
  } else if (I->K == Inst::Var) {
    // copy constant
    if (I->SynthesisConstID != 0) {
      Copy = IC.createVar(I->Width, I->Name, I->Range, I->KnownZeros,
                          I->KnownOnes, I->NonZero, I->NonNegative,
                          I->PowOfTwo, I->Negative, I->NumSignBits,
                          I->SynthesisConstID);
    } else {
      Copy = I;
    }
  } else {
    Copy = IC.getInst(I->K, I->Width, Ops);
  }

  return Copy;
}

void souper::separateBlockPCs(const BlockPCs &BPCs, BlockPCs &BPCsCopy,
                              std::map<Inst *, Inst *> &InstCache,
                              std::map<Block *, Block *> &BlockCache,
                              InstContext &IC,
                              std::map<Inst *, llvm::APInt> *ConstMap,
                              bool CloneVars) {
  for (const auto &BPC : BPCs) {
    auto BPCCopy = BPC;
    BPCCopy.B = BlockCache[BPC.B];
    BPCCopy.PC = InstMapping(getInstCopy(BPC.PC.LHS, IC, InstCache, BlockCache, ConstMap, CloneVars),
                             getInstCopy(BPC.PC.RHS, IC, InstCache, BlockCache, ConstMap, CloneVars));
    BPCsCopy.emplace_back(BPCCopy);
  }
}

void souper::separatePCs(const std::vector<InstMapping> &PCs,
                         std::vector<InstMapping> &PCsCopy,
                         std::map<Inst *, Inst *> &InstCache,
                         std::map<Block *, Block *> &BlockCache,
                         InstContext &IC,
                         std::map<Inst *, llvm::APInt> *ConstMap,
                         bool CloneVars) {
  for (const auto &PC : PCs)
    PCsCopy.emplace_back(getInstCopy(PC.LHS, IC, InstCache, BlockCache, ConstMap, CloneVars),
                         getInstCopy(PC.RHS, IC, InstCache, BlockCache, ConstMap, CloneVars));
}
