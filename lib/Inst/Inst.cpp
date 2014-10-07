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

std::string PrintContext::printInst(Inst *I) {
  std::string Str;
  llvm::raw_string_ostream SS(Str);

  auto PNI = PrintNums.find(static_cast<void *>(I));
  if (PNI != PrintNums.end()) {
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
    unsigned BlockNum = printBlock(I->B);
    OpsSS << " %" << BlockNum << ",";
    break;
  }

  const std::vector<Inst *> &Ops = I->orderedOps();
  for (unsigned I = 0; I != Ops.size(); ++I) {
    if (I == 0)
      OpsSS << " ";
    else
      OpsSS << ", ";
    OpsSS << printInst(Ops[I]);
  }

  unsigned InstNum = PrintNums.size();
  PrintNums[static_cast<void *>(I)] = InstNum;

  Out << "%" << InstNum << ":i" << I->Width << " = " << Inst::getKindName(I->K)
      << OpsSS.str();

  if (!I->Name.empty()) {
    Out << " ; " << I->Name;
  }

  Out << '\n';

  SS << "%" << InstNum;
  return SS.str();
}

unsigned PrintContext::printBlock(Block *B) {
  std::string Str;
  llvm::raw_string_ostream SS(Str);

  auto PNI = PrintNums.find(static_cast<void *>(B));
  if (PNI != PrintNums.end()) {
    return PNI->second;
  }

  unsigned BlockNum = PrintNums.size();
  PrintNums[static_cast<void *>(B)] = BlockNum;

  Out << '%' << BlockNum << " = block\n";
  return BlockNum;
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
  }

  llvm_unreachable("all cases covered");
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

Inst *InstContext::createVar(unsigned Width, llvm::StringRef Name) {
  auto &InstList = VarInstsByWidth[Width];
  unsigned Number = InstList.size();
  auto I = new Inst;
  InstList.emplace_back(I);

  I->K = Inst::Var;
  I->Number = Number;
  I->Width = Width;
  I->Name = Name;
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
  if (Inst::isAssociative(K)) {
    for (Inst *Op : Ops) {
      if (Op->K == K)
        OrderedOps.insert(OrderedOps.end(), Op->Ops.begin(), Op->Ops.end());
      else
        OrderedOps.push_back(Op);
    }
    std::sort(OrderedOps.begin(), OrderedOps.end());
    InstOps = &OrderedOps;
  } else if (Inst::isCommutative(K)) {
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

bool Inst::isAssociative(Inst::Kind K) {
  switch (K) {
  case Add:
  case Mul:
  case And:
  case Or:
  case Xor:
    return true;
  default:
    return false;
  }
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

void souper::PrintReplacement(llvm::raw_ostream &Out,
                              const std::vector<InstMapping> &PCs,
                              InstMapping Mapping) {
  assert(Mapping.LHS);
  assert(Mapping.RHS);

  PrintContext Printer(Out);
  for (const auto &PC : PCs) {
    std::string SRef = Printer.printInst(PC.LHS);
    std::string RRef = Printer.printInst(PC.RHS);
    Out << "pc " << SRef << " " << RRef << '\n';
  }

  std::string SRef = Printer.printInst(Mapping.LHS);
  std::string RRef = Printer.printInst(Mapping.RHS);
  Out << "cand " << SRef << " " << RRef << '\n';
}

std::string souper::GetReplacementString(const std::vector<InstMapping> &PCs,
                                         InstMapping Mapping) {
  std::string Str;
  llvm::raw_string_ostream SS(Str);
  PrintReplacement(SS, PCs, Mapping);
  return SS.str();
}

void souper::PrintReplacementLHS(llvm::raw_ostream &Out,
                                 const std::vector<InstMapping> &PCs,
                                 Inst *LHS) {
  assert(LHS);

  PrintContext Printer(Out);
  for (const auto &PC : PCs) {
    std::string SRef = Printer.printInst(PC.LHS);
    std::string RRef = Printer.printInst(PC.RHS);
    Out << "pc " << SRef << " " << RRef << '\n';
  }

  std::string SRef = Printer.printInst(LHS);
  Out << "infer " << SRef << '\n';
}

std::string souper::GetReplacementLHSString(const std::vector<InstMapping> &PCs,
                                            Inst *LHS) {
  std::string Str;
  llvm::raw_string_ostream SS(Str);
  PrintReplacementLHS(SS, PCs, LHS);
  return SS.str();
}

void souper::PrintReplacementRHS(llvm::raw_ostream &Out, llvm::APInt Const) {
  Out << "result " << Const << ":i" << Const.getBitWidth() << '\n';
}

std::string souper::GetReplacementRHSString(llvm::APInt Const) {
  std::string Str;
  llvm::raw_string_ostream SS(Str);
  PrintReplacementRHS(SS, Const);
  return SS.str();
}
