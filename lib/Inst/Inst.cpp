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

using namespace souper;

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

  llvm_unreachable("Should have found an unequal operand");
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
        OpsSS << printInst(Ops[Idx], Out, printNames);
        break;
      case Inst::SAddWithOverflow:
      case Inst::UAddWithOverflow:
      case Inst::SSubWithOverflow:
      case Inst::USubWithOverflow:
      case Inst::SMulWithOverflow:
      case Inst::UMulWithOverflow:
        OpsSS << printInst(I->Ops[1]->Ops[Idx], Out, printNames);
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
      if (I->K == Inst::Var && (I->KnownZeros.getBoolValue() ||
                                I->KnownOnes.getBoolValue())) {
        Out << " (" << Inst::getKnownBitsString(I->KnownZeros, I->KnownOnes)
            << ")" << OpsSS.str();
      } else {
        Out << OpsSS.str();
      }
      if (printNames && !I->Name.empty()) {
        Out << " ; " << I->Name;
      }
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
  for (int K=Zero.getBitWidth()-1; K>=0; --K) {
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
  case Cttz:
    return "cttz";
  case Ctlz:
    return "ctlz";
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
  case SAddO:
    break;
  case UAddO:
    break;
  case SSubO:
    break;
  case USubO:
    break;
  case SMulO:
    break;
  case UMulO:
    break;
  }

  llvm_unreachable("all cases covered");
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
                   .Case("cttz", Inst::Cttz)
                   .Case("ctlz", Inst::Ctlz)
                   .Case("sadd.with.overflow", Inst::SAddWithOverflow)
                   .Case("uadd.with.overflow", Inst::UAddWithOverflow)
                   .Case("ssub.with.overflow", Inst::SSubWithOverflow)
                   .Case("usub.with.overflow", Inst::USubWithOverflow)
                   .Case("smul.with.overflow", Inst::SMulWithOverflow)
                   .Case("umul.with.overflow", Inst::UMulWithOverflow)
                   .Case("extractvalue", Inst::ExtractValue)
                   .Default(Inst::Kind(~0));
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

Inst *InstContext::createVar(unsigned Width, llvm::StringRef Name,
                             llvm::APInt Zero, llvm::APInt One) {
  auto &InstList = VarInstsByWidth[Width];
  unsigned Number = InstList.size();
  auto I = new Inst;
  InstList.emplace_back(I);

  I->K = Inst::Var;
  I->Number = Number;
  I->Width = Width;
  I->Name = Name;
  I->KnownZeros = Zero;
  I->KnownOnes = One;
  return I;
}

Block *InstContext::createBlock(unsigned Preds) {
  auto &BlockList = BlocksByPreds[Preds];
  unsigned Number = BlockList.size();
  auto B = new Block;
  BlockList.emplace_back(B);

  B->Number = Number;
  B->Preds = Preds;
  return B;
}

Inst *InstContext::getPhi(Block *B, const std::vector<Inst *> &Ops) {
  llvm::FoldingSetNodeID ID;
  ID.AddInteger(Inst::Phi);
  ID.AddInteger(Ops[0]->Width);
  ID.AddPointer(B);
  for (auto O : Ops)
    ID.AddPointer(O);

  void *IP = 0;
  if (Inst *I = InstSet.FindNodeOrInsertPos(ID, IP))
    return I;

  auto N = new Inst;
  Insts.emplace_back(N);
  N->K = Inst::Phi;
  N->Width = Ops[0]->Width;
  N->B = B;
  N->Ops = Ops;
  InstSet.InsertNode(N, IP);
  return N;
}

Inst *InstContext::getInst(Inst::Kind K, unsigned Width,
                           const std::vector<Inst *> &Ops) {
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

  void *IP = 0;
  if (Inst *I = InstSet.FindNodeOrInsertPos(ID, IP))
    return I;

  auto N = new Inst;
  Insts.emplace_back(N);
  N->K = K;
  N->Width = Width;
  N->Ops = *InstOps;
  InstSet.InsertNode(N, IP);
  return N;
}

bool Inst::isCommutative(Inst::Kind K) {
  switch (K) {
  case Add:
  case AddNSW:
  case AddNUW:
  case AddNW:
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
      return 5;
    default:
      return 1;
  }
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
  Out << "cand " << SRef << " " << RRef << '\n';
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
  Out << "infer " << SRef << '\n';
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
