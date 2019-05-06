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

#include "souper/Parser/Parser.h"

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "souper/Extractor/Candidates.h"
#include "souper/Inst/Inst.h"

#include <string>
#include <unordered_set>

using namespace llvm;
using namespace souper;

namespace {

struct Token {
  enum Kind {
    Error,

    Ident,
    ValName,
    Comma,
    Eq,
    Int,
    UntypedInt,
    KnownBits,
    OpenParen,
    CloseParen,
    OpenBracket,
    Eof,
  };

  Kind K;
  const char *Pos;
  size_t Len;
  APInt Val;
  StringRef Name;
  unsigned Width;
  std::string PatternString;

  StringRef str() const {
    return StringRef(Pos, Len);
  }
};

struct TokenPos {
  unsigned Line, Col;
};

struct Lexer {
  const char *Begin, *End;

  const char *LineBegin;
  unsigned LineNum;

  Lexer(const char *Begin, const char *End)
      : Begin(Begin), End(End), LineBegin(Begin), LineNum(1) {}

  Token getNextToken(std::string &ErrStr);

  TokenPos getTokenPos(const Token &T) {
    return {LineNum, unsigned(T.Pos - LineBegin + 1)};
  }
};

}

Token Lexer::getNextToken(std::string &ErrStr) {
  while (Begin != End) {
    switch (*Begin) {
    case '\n':
      LineNum++;
      LineBegin = ++Begin;
      continue;

    case ' ':
    case '\t':
    case '\r':
      ++Begin;
      continue;

    case ';':
      while (Begin != End && *Begin != '\n')
        ++Begin;
      continue;

    default:
      goto FoundChar;
    }
  }

  return Token{Token::Eof, Begin, 0, APInt()};

FoundChar:
  switch (*Begin) {
    case ',':
      ++Begin;
      return Token{Token::Comma, Begin-1, 1, APInt()};
    case '=':
      ++Begin;
      return Token{Token::Eq, Begin-1, 1, APInt()};
    case '%': {
      ++Begin;
      unsigned Width = 0;
      const char *NameBegin = Begin;
      while (Begin != End && ((*Begin >= '0' && *Begin <= '9') ||
                              (*Begin >= 'A' && *Begin <= 'Z') ||
                              (*Begin >= 'a' && *Begin <= 'z') ||
                              (*Begin == '_'))) {
        ++Begin;
      }
      if (Begin == NameBegin) {
        ErrStr = "expected identifier";
        return Token{Token::Error, Begin, 0, APInt()};
      }
      const char *NameEnd = Begin;
      if (Begin != End && *Begin == ':') {
        ++Begin;
        if (Begin == End || *Begin != 'i') {
          ErrStr = "expected 'i'";
          return Token{Token::Error, Begin, 0, APInt()};
        }

        ++Begin;
        const char *WidthBegin = Begin;
        while (Begin != End && *Begin >= '0' && *Begin <= '9') {
          Width = Width*10 + (*Begin - '0');
          ++Begin;
        }
        if (Begin == WidthBegin) {
          ErrStr = "expected integer";
          return Token{Token::Error, Begin, 0, APInt()};
        }
        if (Width == 0) {
          ErrStr = "width must be at least 1";
          return Token{Token::Error, WidthBegin, 0, APInt()};
        }
      }

      Token T;
      T.K = Token::ValName;
      T.Pos = NameBegin - 1;
      T.Len = Begin - NameBegin + 1;
      T.Name = StringRef(NameBegin, NameEnd - NameBegin);
      T.Width = Width;
      return T;
    }
  }

  if ((*Begin >= 'a' && *Begin <= 'z') ||
      (*Begin >= 'A' && *Begin <= 'Z')) {
    const char *TokenBegin = Begin;
    do {
      ++Begin;
    } while (Begin != End && ((*Begin >= 'a' && *Begin <= 'z') ||
             (*Begin == '.') || (*Begin >= 'A' && *Begin <= 'Z')));
    std::string DataFlowFact = StringRef(TokenBegin, Begin - TokenBegin);
    if (DataFlowFact == "knownBits") {
      if (*Begin != '=') {
        ErrStr = "expected '=' for knownBits";
        return Token{Token::Error, Begin, 0, APInt()};
      }
      ++Begin;
      const char *PatternBegin = Begin;
      while (*Begin == '0' || *Begin == '1' || *Begin == 'x')
        ++Begin;
      if (Begin == PatternBegin) {
        ErrStr = "expected [0|1|x]+ for knownBits";
        return Token{Token::Error, Begin, 0, APInt()};
      }
      return Token{Token::KnownBits, TokenBegin, size_t(Begin - TokenBegin), APInt(),
                   "", 0, StringRef(PatternBegin, Begin - PatternBegin)};
    } else
      return Token{Token::Ident, TokenBegin, size_t(Begin - TokenBegin), APInt()};
  }

  if (*Begin == '-' || (*Begin >= '0' && *Begin <= '9')) {
    const char *NumBegin = Begin;
    do {
      ++Begin;
    } while (Begin != End && *Begin >= '0' && *Begin <= '9');
    const char *NumEnd = Begin;
    if ((NumEnd - NumBegin) == 1 && *NumBegin == '-') {
      ErrStr = "unexpected character following a negative sign";
      return Token{Token::Error, Begin, 0, APInt()};
    }
    if (Begin != End && *Begin == ':') {
      ++Begin;
      if (Begin == End || *Begin != 'i') {
        ErrStr = "expected 'i'";
        return Token{Token::Error, Begin, 0, APInt()};
      }
      ++Begin;
      unsigned Width = 0;
      const char *WidthBegin = Begin;
      while (Begin != End && *Begin >= '0' && *Begin <= '9') {
        Width = Width*10 + (*Begin - '0');
        ++Begin;
      }
      if (Begin == WidthBegin) {
        ErrStr = "expected integer";
        return Token{Token::Error, Begin, 0, APInt()};
      }
      if (Width == 0) {
        ErrStr = "width must be at least 1";
        return Token{Token::Error, WidthBegin, 0, APInt()};
      }
      // this calculation is from an assertion in APInt::fromString()
      if ((((NumEnd - NumBegin) - 1) * 64) / 22 > Width) {
        ErrStr = "integer constant is too large for its width";
        return Token{Token::Error, Begin, 0, APInt()};
      }
      return Token{Token::Int, NumBegin, size_t(Begin - NumBegin),
                   APInt(Width, StringRef(NumBegin, NumEnd - NumBegin), 10)};
    }

    return Token{Token::UntypedInt, NumBegin, size_t(Begin - NumBegin),
                 APInt((NumEnd - NumBegin) * 5,
                        StringRef(NumBegin, NumEnd - NumBegin), 10)};
  }

  if (*Begin == '(') {
    ++Begin;
    return Token{Token::OpenParen, Begin-1, 1, APInt()};
  }
  if (*Begin == ')') {
    ++Begin;
    return Token{Token::CloseParen, Begin-1, 1, APInt()};
  }

  if (*Begin == '[') {
    ++Begin;
    return Token{Token::OpenBracket, Begin-1, 1, APInt()};
  }

  ErrStr = std::string("unexpected '") + *Begin + "'";
  ++Begin;
  return Token{Token::Error, Begin-1, 0, APInt()};
}

void souper::TestLexer(StringRef Str) {
  Lexer L(Str.data(), Str.data() + Str.size());

  while (1) {
    std::string ErrStr;
    Token T = L.getNextToken(ErrStr);

    if (T.K == Token::Eof)
      break;

    TokenPos TP = L.getTokenPos(T);
    llvm::errs() << TP.Line << ":" << TP.Col << ": " << T.K << " '"
                 << StringRef(T.Pos, T.Len) << "'\n";

    if (T.K == Token::Error)
      llvm::errs() << ErrStr << "\n";
  }
}

namespace {

enum class ReplacementKind {
  ParseLHS,
  ParseRHS,
  ParseBoth,
};

struct Parser {
  Parser(StringRef FileName, StringRef Str, InstContext &IC,
         std::vector<ParsedReplacement> &Reps, ReplacementKind RK,
         std::vector<ReplacementContext> *RCsIn,
         std::vector<ReplacementContext> *RCsOut)
      : FileName(FileName),
        L(Str.data(), Str.data() + Str.size()),
        IC(IC),
        Reps(Reps),
        RK(RK),
        RCsIn(RCsIn),
        RCsOut(RCsOut) {
    if (RCsIn)
      Context = (*RCsIn)[0];
  }

  std::string FileName;
  Lexer L;
  Token CurTok;
  InstContext &IC;
  std::vector<ParsedReplacement> &Reps;
  ReplacementKind RK;
  std::vector<ReplacementContext> *RCsIn, *RCsOut;
  ReplacementContext Context;
  int Index = 0;
  int ReservedConstCounter = 0;

  std::vector<InstMapping> PCs;
  BlockPCs BPCs;
  // When we finish, BlockPCIdxMap[B] is the largest PredIdx for the
  // set of blockpc(s) related to B. The map is used for error- and
  // type-checking.
  std::map<Block *, unsigned> BlockPCIdxMap;
  std::unordered_set<Inst *> ExternalUsesSet;
  Inst *LHS = 0;

  std::string makeErrStr(const std::string &ErrStr) {
    return makeErrStr(L.getTokenPos(CurTok), ErrStr);
  }

  std::string makeErrStr(TokenPos TP, const std::string &ErrStr) {
    return FileName + ":" + utostr(TP.Line) + ":" + utostr(TP.Col) + ": " +
           ErrStr;
  }

  bool lossy(const APInt &I, unsigned NewWidth) {
    unsigned W = I.getBitWidth();
    if (NewWidth >= W)
      return false;
    auto NI = I.trunc(NewWidth);
    return NI.zext(W) != I && NI.sext(W) != I;
  }

  bool consumeToken(std::string &ErrStr) {
    CurTok = L.getNextToken(ErrStr);
    if (CurTok.K == Token::Error) {
      ErrStr = makeErrStr(ErrStr);
      return false;
    } else {
      return true;
    }
  }

  Inst *parseInst(std::string &ErrStr);
  InstMapping parseInstMapping(std::string &ErrStr);

  bool typeCheckPhi(unsigned Width, Block *B, std::vector<Inst *> &Ops,
                    std::string &ErrStr);
  bool typeCheckInst(Inst::Kind IK, unsigned &Width, std::vector<Inst *> &Ops,
                     std::string &ErrStr);
  bool typeCheckOpsMatchingWidths(llvm::MutableArrayRef<Inst *> Ops,
                                  std::string &ErrStr);

  bool parseLine(std::string &ErrStr);

  ParsedReplacement parseReplacement(std::string &ErrStr);
  std::vector<ParsedReplacement> parseReplacements(std::string &ErrStr);
  void nextReplacement();
  bool parseInstAttribute(std::string &ErrStr, Inst *LHS);
  bool isOverflow(Inst::Kind IK);
};

}

Inst *Parser::parseInst(std::string &ErrStr) {
  switch (CurTok.K) {
    case Token::ValName: {
      if (CurTok.Width != 0) {
        ErrStr = makeErrStr("inst reference may not have a width");
        return 0;
      }
      Inst *I = Context.getInst(CurTok.Name);
      if (!I) {
        ErrStr = makeErrStr(std::string("%") + CurTok.Name.str() +
                            " is not an inst");
        return 0;
      }
      if (!consumeToken(ErrStr))
        return 0;
      return I;
    }

    case Token::Int: {
      Inst *I = IC.getConst(CurTok.Val);
      if (!consumeToken(ErrStr))
        return 0;
      return I;
    }

    case Token::UntypedInt: {
      Inst *I = IC.getUntypedConst(CurTok.Val);
      if (!consumeToken(ErrStr))
        return 0;
      return I;
    }

    default: {
      ErrStr = makeErrStr("unexpected token: '" +
                          std::string(CurTok.str()) + "'");
      return 0;
    }
  }
}

bool Parser::isOverflow(Inst::Kind IK) {
  return (IK == Inst::SAddWithOverflow || IK == Inst::UAddWithOverflow ||
          IK == Inst::SSubWithOverflow || IK == Inst::USubWithOverflow ||
          IK == Inst::SMulWithOverflow || IK == Inst::UMulWithOverflow);
}

bool Parser::typeCheckOpsMatchingWidths(llvm::MutableArrayRef<Inst *> Ops,
                                        std::string &ErrStr) {
  unsigned Width = 0;
  for (auto Op : Ops) {
    if (Width == 0)
      Width = Op->Width;
    if (Width != 0 && Op->Width != 0 && Width != Op->Width) {
      ErrStr = "operands have different widths";
      return false;
    }
  }

  if (Width == 0) {
    ErrStr = "at least one operand must be typed";
    return false;
  }

  for (auto &Op : Ops) {
    if (Op->Width == 0) {
      if (lossy(Op->Val, Width)) {
        ErrStr = "integer constant is too large for its width";
        return false;
      }
      Op = IC.getConst(Op->Val.sextOrTrunc(Width));
    }
  }
  return true;
}

bool Parser::typeCheckPhi(unsigned Width, Block *B,
                          std::vector<Inst *> &Ops, std::string &ErrStr) {
  if (B->Preds != Ops.size()) {
    ErrStr = "phi has " + utostr(Ops.size()) +
      " operand(s) but preceding block has " + utostr(B->Preds);
    return false;
  }
  auto IdxIt = BlockPCIdxMap.find(B);
  if (IdxIt != BlockPCIdxMap.end() && IdxIt->second >= Ops.size()) {
    ErrStr = "blockpc's predecessor number is larger "
             "than the number of phi's operands";
    return false;
  }

  if (!typeCheckOpsMatchingWidths(Ops, ErrStr))
    return false;

  if (Width != 0 && Width != Ops[0]->Width) {
    ErrStr = "inst must have width of " + utostr(Ops[0]->Width) +
                         ", has width " + utostr(Width);
    return false;
  }

  for (auto Op : Ops) {
    if (isOverflow(Op->K)) {
      ErrStr = "overflow intrinsic cannot be an operand of phi instruction";
      return false;
    }
  }

  return true;
}

bool Parser::typeCheckInst(Inst::Kind IK, unsigned &Width,
                           std::vector<Inst *> &Ops,
                           std::string &ErrStr) {
  unsigned MinOps = 2, MaxOps = 2;
  llvm::MutableArrayRef<Inst *> OpsMatchingWidths = Ops;

  switch (IK) {
  case Inst::Const:
  case Inst::UntypedConst:
  case Inst::Var:
  case Inst::Phi:
    llvm_unreachable("should not be handled here");

  case Inst::Add:
  case Inst::Mul:
  case Inst::And:
  case Inst::Or:
  case Inst::Xor:
    break;

  case Inst::AddNSW:
  case Inst::AddNUW:
  case Inst::AddNW:
  case Inst::Sub:
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
  case Inst::Eq:
  case Inst::Ne:
  case Inst::Ult:
  case Inst::Slt:
  case Inst::Ule:
  case Inst::Sle:
    MaxOps = 2;
    break;

  case Inst::SAddO:
  case Inst::UAddO:
  case Inst::SSubO:
  case Inst::USubO:
  case Inst::SMulO:
  case Inst::UMulO:
  case Inst::SAddWithOverflow:
  case Inst::UAddWithOverflow:
  case Inst::SSubWithOverflow:
  case Inst::USubWithOverflow:
  case Inst::SMulWithOverflow:
  case Inst::UMulWithOverflow:
  case Inst::ExtractValue:
  case Inst::SAddSat:
  case Inst::UAddSat:
  case Inst::SSubSat:
  case Inst::USubSat:
    MinOps = MaxOps = 2;
    break;

  case Inst::Select:
    MinOps = MaxOps = 3;
    if (Ops.size() > 1) {
      if (Ops[0]->Width == 0) {
        if (lossy(Ops[0]->Val, 1)) {
          ErrStr = "integer constant is too large for its width";
          return false;
        }
        Ops[0] = IC.getConst(Ops[0]->Val.trunc(1));
      } else if (Ops[0]->Width != 1) {
        ErrStr = std::string("first operand must have width of 1, has width ") +
                 utostr(Ops[0]->Width);
        return false;
      }
      OpsMatchingWidths =
          llvm::MutableArrayRef<Inst *>(Ops.data() + 1, Ops.size() - 1);
    }
    break;

  case Inst::ZExt:
  case Inst::SExt:
    MinOps = MaxOps = 1;
    if (Ops.size() == 1 && Width != 0 && Width <= Ops[0]->Width) {
      ErrStr = std::string("inst must have width of at least ") +
               utostr(Ops[0]->Width + 1) + ", has width " + utostr(Width);
      return false;
    }
    break;

  case Inst::Trunc:
    MinOps = MaxOps = 1;
    if (Ops.size() == 1 && Width != 0 && Width >= Ops[0]->Width) {
      ErrStr = std::string("inst must have width of at most ") +
               utostr(Ops[0]->Width - 1) + ", has width " + utostr(Width);
      return false;
    }
    break;

  case Inst::CtPop:
  case Inst::BSwap:
  case Inst::BitReverse:
  case Inst::Cttz:
  case Inst::Ctlz:
    MaxOps = MinOps = 1;
    break;
  case Inst::FShl:
  case Inst::FShr:
    MaxOps = MinOps = 3;
    break;

  default:
    llvm::report_fatal_error("unhandled");
  }

  if (MinOps == MaxOps && Ops.size() != MinOps) {
    ErrStr = std::string("expected ") + utostr(MinOps) + " operands, found " +
             utostr(Ops.size());
    return false;
  }

  if (Ops.size() < MinOps) {
    ErrStr = std::string("expected at least ") + utostr(MinOps) +
             " operands, found " + utostr(Ops.size());
    return false;
  }

  if (Ops.size() > MaxOps) {
    ErrStr = std::string("expected at most ") + utostr(MaxOps) +
             " operands, found " + utostr(Ops.size());
    return false;
  }

  // NOTE: We don't 'typeCheckOpsMatchingWidths' for ExtractValue
  // instruction because its a tuple of {aggregate, index}. The first
  // element, aggregate is result of overflow instruction. The
  // aggregate is of 33 and 65 bits for 32 and 64 bit operands of
  // overflow instruction respectively. The second element of
  // ExtractValue instruction is an index value. We don't type check
  // the operands width as the two elements vary in width.
  if (IK != Inst::ExtractValue) {
    if (!typeCheckOpsMatchingWidths(OpsMatchingWidths, ErrStr))
      return false;

    for (auto Op : Ops) {
      if (isOverflow(Op->K)) {
        ErrStr = std::string("overflow intrinsic cannot be an operand of ") + Inst::getKindName(IK) +
                 std::string(" instruction");
        return false;
      }
    }
  }

  unsigned ExpectedWidth;
  switch (IK) {
  case Inst::ZExt:
  case Inst::SExt:
  case Inst::Trunc:
    ExpectedWidth = 0;
    break;

  case Inst::Select:
    ExpectedWidth = Ops[1]->Width;
    break;

  case Inst::Eq:
  case Inst::Ne:
  case Inst::Ult:
  case Inst::Slt:
  case Inst::Ule:
  case Inst::Sle:
    ExpectedWidth = 1;
    break;

  case Inst::SAddO:
  case Inst::UAddO:
  case Inst::SSubO:
  case Inst::USubO:
  case Inst::SMulO:
  case Inst::UMulO:
    ExpectedWidth = 1;
    break;

  case Inst::SAddWithOverflow:
  case Inst::UAddWithOverflow:
  case Inst::SSubWithOverflow:
  case Inst::USubWithOverflow:
  case Inst::SMulWithOverflow:
  case Inst::UMulWithOverflow:
    ExpectedWidth = Ops[0]->Width + 1;
    break;

  case Inst::ExtractValue:
    if (Ops[1]->Val == 0) {
      switch (Ops[0]->K) {
        case Inst::SAddWithOverflow:
        case Inst::UAddWithOverflow:
        case Inst::SSubWithOverflow:
        case Inst::USubWithOverflow:
        case Inst::SMulWithOverflow:
        case Inst::UMulWithOverflow:
          ExpectedWidth = Ops[0]->Ops[0]->Width;
          break;
        default:
          ErrStr = "extract value expects an aggregate type";
          return false;
      }
    } else if (Ops[1]->Val == 1) {
      switch (Ops[0]->K) {
        case Inst::SAddWithOverflow:
        case Inst::UAddWithOverflow:
        case Inst::SSubWithOverflow:
        case Inst::USubWithOverflow:
        case Inst::SMulWithOverflow:
        case Inst::UMulWithOverflow:
          ExpectedWidth = 1;
          break;
        default:
          ErrStr = "extract value expects an aggregate type";
          return false;
      }
    } else {
      ErrStr = "extractvalue inst doesn't expect index value other than 0 or 1";
      return false;
    }
    break;

  default:
    ExpectedWidth = Ops[0]->Width;
    break;
  }

  if (Width == 0) {
    if (ExpectedWidth == 0) {
      ErrStr = "inst must have a width";
      return false;
    }
    Width = ExpectedWidth;
  } else if (ExpectedWidth != 0 && Width != ExpectedWidth) {
    ErrStr = std::string("inst must have width of ") + utostr(ExpectedWidth) +
             ", has width " + utostr(Width);
    return false;
  }

  if (IK == Inst::BSwap && Width % 16 != 0) {
    ErrStr = "bswap argument must be a multiple of 16 bits";
    return false;
  }

  return true;
}

void Parser::nextReplacement() {
  PCs.clear();
  BPCs.clear();
  BlockPCIdxMap.clear();
  if (RCsOut)
    RCsOut->emplace_back(Context);
  ++Index;
  if (RCsIn)
    Context = (*RCsIn)[Index];
  else
    Context.clear();
  LHS = 0;
}

InstMapping Parser::parseInstMapping(std::string &ErrStr) {
  Inst *SrcRep[2];
  SrcRep[0] = parseInst(ErrStr);
  if (!SrcRep[0])
    return InstMapping();

  SrcRep[1] = parseInst(ErrStr);
  if (!SrcRep[1])
    return InstMapping();

  if (!typeCheckOpsMatchingWidths(SrcRep, ErrStr)) {
    ErrStr = makeErrStr(ErrStr);
    return InstMapping();
  }

  return InstMapping(SrcRep[0], SrcRep[1]);
}

bool Parser::parseInstAttribute(std::string &ErrStr, Inst *LHS) {
  int DemandedBitsCount = 0;
  int HarvestKindCount = 0;
  LHS->HarvestKind = HarvestType::HarvestedFromDef;
  LHS->DemandedBits = APInt::getAllOnesValue(LHS->Width);
  while (CurTok.K == Token::OpenParen) {
    llvm::APInt DemandedBitsVal = APInt(LHS->Width, 0, false);
    llvm::APInt ConstOne(LHS->Width, 1, false);
    if (!consumeToken(ErrStr))
      return false;
    if (CurTok.K != Token::Ident) {
      ErrStr = makeErrStr("missing Inst attribute string");
      return false;
    }
    if (CurTok.str() == "demandedBits") {
      DemandedBitsCount++;
      if (DemandedBitsCount > 1) {
        ErrStr = makeErrStr("only one demandedbits attribute is allowed");
        return false;
      }
      if (!consumeToken(ErrStr))
        return false;
      if (CurTok.K != Token::Eq) {
        ErrStr = makeErrStr("expected '=' for demandedBits");
        return false;
      }
      if (!consumeToken(ErrStr))
        return false;
      if (CurTok.K != Token::UntypedInt) {
        ErrStr = makeErrStr("expected demandedBits pattern of type [0|1]+");
        return false;
      }
      if (LHS->Width != CurTok.Len) {
        ErrStr = makeErrStr("demandedBits pattern must be of same length as infer operand width");
        return false;
      }
      std::string DemandedBitsPattern = CurTok.str();
      for (unsigned i = 0; i < LHS->Width; ++i) {
        if (DemandedBitsPattern[i] == '1') {
          DemandedBitsVal += ConstOne.shl(DemandedBitsPattern.length() - 1 - i);
        } else if (DemandedBitsPattern[i] != '0') {
          ErrStr = makeErrStr("expected demandedBits pattern of type [0|1]+");
          return false;
        }
      }
      if (!consumeToken(ErrStr))
        return false;
      if (CurTok.K != Token::CloseParen) {
        ErrStr = makeErrStr("expected ')' to complete demandedBits");
        return false;
      }
      if (!consumeToken(ErrStr))
        return false;
      LHS->DemandedBits = DemandedBitsVal;
    } else if (CurTok.str() == "harvestedFromUse") {
      HarvestKindCount++;
      if (HarvestKindCount > 1) {
        ErrStr = makeErrStr("only one harvestFromUse attribute is allowed");
        return false;
      }
      if (!consumeToken(ErrStr))
        return false;
      if (CurTok.K != Token::CloseParen) {
        ErrStr = makeErrStr("expected ')' to complete harvestFromUse");
        return false;
      }
      if (!consumeToken(ErrStr))
        return false;
      LHS->HarvestKind = HarvestType::HarvestedFromUse;
    } else {
      ErrStr = makeErrStr("invalid Inst attribute string");
      return false;
    }
  }
  return true;
}

bool Parser::parseLine(std::string &ErrStr) {
  switch (CurTok.K) {
    case Token::Ident:
      if (CurTok.str() == "cand") {
        if (RK == ReplacementKind::ParseLHS) {
          ErrStr = makeErrStr("Not expecting 'cand' when parsing LHS");
          return false;
        }
        if (RK == ReplacementKind::ParseRHS) {
          ErrStr = makeErrStr("Not expecting 'cand' when parsing RHS");
          return false;
        }
        if (!consumeToken(ErrStr)) return false;
        InstMapping Cand = parseInstMapping(ErrStr);
        if (!ErrStr.empty()) return false;
        if (!parseInstAttribute(ErrStr, Cand.LHS))
          return false;

        if (isOverflow(Cand.LHS->K) || isOverflow(Cand.RHS->K)) {
          ErrStr = makeErrStr("overflow intrinsic cannot be an operand of cand instruction");
          return false;
        }

        Reps.push_back(ParsedReplacement{Cand, std::move(PCs),
                                         std::move(BPCs)});
        nextReplacement();

        return true;
      } else if (CurTok.str() == "infer") {
        if (RK == ReplacementKind::ParseRHS) {
          ErrStr = makeErrStr("Not expecting 'infer' when parsing RHS");
          return false;
        }
        if (!consumeToken(ErrStr)) return false;
        if (CurTok.K != Token::ValName) {
          ErrStr = makeErrStr("unexpected infer operand type");
          return false;
        }
        if (LHS) {
          ErrStr = makeErrStr("Not expecting a second 'infer'");
          return false;
        }
        LHS = parseInst(ErrStr);
        if (!LHS)
          return false;
        if (!parseInstAttribute(ErrStr, LHS))
          return false;

        if (isOverflow(LHS->K)) {
          ErrStr = makeErrStr("overflow intrinsic cannot be an operand of infer instruction");
          return false;
        }

        if (RK == ReplacementKind::ParseLHS) {
          Reps.push_back(ParsedReplacement{InstMapping(LHS, 0),
                                           std::move(PCs), std::move(BPCs)});
          nextReplacement();
        }

        return true;
      } else if (CurTok.str() == "result") {
        if (RK == ReplacementKind::ParseLHS) {
          ErrStr = makeErrStr("Not expecting 'result' when parsing LHS");
          return false;
        }
        if (RK != ReplacementKind::ParseRHS && !LHS) {
          ErrStr = makeErrStr("Not expecting 'result' before 'infer'");
          return false;
        }
        if (!consumeToken(ErrStr)) return false;
        if (CurTok.K == Token::UntypedInt) {
          ErrStr = makeErrStr("expecting width to be specified for int operand in 'result' instruction");
          return false;
        }
        Inst *RHS = parseInst(ErrStr);
        if (!RHS)
          return false;
        if (LHS && (LHS->Width != RHS->Width)) {
          ErrStr = makeErrStr("width of result and infer operands mismatch");
          return false;
        }
        InstMapping Cand = InstMapping(LHS, RHS);

        Reps.push_back(ParsedReplacement{Cand, std::move(PCs),
                                         std::move(BPCs)});
        nextReplacement();

        return true;
      } else if (CurTok.str() == "pc") {
        if (!consumeToken(ErrStr)) return false;
        InstMapping PC = parseInstMapping(ErrStr);
        if (!ErrStr.empty()) return false;

        if (isOverflow(PC.LHS->K)) {
          ErrStr = makeErrStr("overflow intrinsic cannot be an operand of pc instruction");
          return false;
        }

        PCs.push_back(PC);

        return true;
      } else if (CurTok.str() == "blockpc") {
        if (!consumeToken(ErrStr)) return false;
        if (CurTok.K != Token::ValName) {
          ErrStr = makeErrStr("expected block var");
          return false;
        }
        StringRef InstName = CurTok.Name;
        unsigned InstWidth = CurTok.Width;
        if (InstWidth != 0) {
          ErrStr = makeErrStr("blocks may not have a width");
          return false;
        }
        if (Context.getInst(CurTok.Name)) {
          ErrStr = makeErrStr(std::string("%") + InstName.str() +
                            " is declared as an inst");
          return false;
        }
        Block *B = Context.getBlock(InstName);
        if (B == 0) {
          ErrStr = makeErrStr(std::string("block %") + InstName.str() +
                              " is undeclared");
          return false;
        }
        if (!consumeToken(ErrStr)) return false;

        if (CurTok.K != Token::UntypedInt) {
          ErrStr = makeErrStr(std::string("expected block number"));
          return false;
        }
        unsigned CurrIdx = CurTok.Val.getLimitedValue();
        if (!consumeToken(ErrStr)) return false;

        InstMapping PC = parseInstMapping(ErrStr);
        if (!ErrStr.empty()) return false;

        if (isOverflow(PC.LHS->K)) {
          ErrStr = makeErrStr("overflow intrinsic cannot be an operand of blockpc instruction");
          return false;
        }

        std::map<Block *, unsigned>::iterator IdxIt = BlockPCIdxMap.find(B);
        if (IdxIt == BlockPCIdxMap.end()) {
          BlockPCIdxMap[B] = CurrIdx;
        }
        else {
          assert(BPCs.size() && "Empty BlockPCs!");
          if (CurrIdx > IdxIt->second)
            BlockPCIdxMap[B] = CurrIdx;
        }
        BPCs.emplace_back(B, CurrIdx, PC);

        return true;
      } else {
        ErrStr = makeErrStr(std::string("unexpected identifier: '") +
                            CurTok.str().str() + "'");
        return false;
      }

    case Token::ValName: {
      StringRef InstName = CurTok.Name;
      unsigned InstWidth = CurTok.Width;

      if (Context.getInst(InstName)) {
        ErrStr = makeErrStr(std::string("%") + InstName.str() +
                            " already declared as an inst");
        return false;
      }
      if (Context.getBlock(InstName)) {
        ErrStr = makeErrStr(std::string("%") + InstName.str() +
                            " already declared as a block");
        return false;
      }

      TokenPos TP = L.getTokenPos(CurTok);
      if (!consumeToken(ErrStr)) return false;

      if (CurTok.K != Token::Eq) {
        ErrStr = makeErrStr("expected '='");
        return false;
      }
      if (!consumeToken(ErrStr)) return false;

      if (CurTok.K != Token::Ident) {
        ErrStr = makeErrStr("expected identifier");
        return false;
      }

      Inst::Kind IK = Inst::getKind(CurTok.str());

      if (IK == Inst::None) {
        if (CurTok.str() == "block") {
          if (InstWidth != 0) {
            ErrStr = makeErrStr(TP, "blocks may not have a width");
            return false;
          }

          if (!consumeToken(ErrStr))
            return false;
          if (CurTok.K != Token::UntypedInt) {
            ErrStr = makeErrStr(TP, "block must be followed by number of preds");
            return false;
          }
          unsigned Preds = CurTok.Val.getLimitedValue();
          if (Preds > MaxPreds) {
            ErrStr = makeErrStr(std::string(std::to_string(Preds) +
                                            " is too many block predecessors"));
            return false;
          }
          Context.setBlock(InstName, IC.createBlock(Preds));
          return consumeToken(ErrStr);
        } else {
          ErrStr = makeErrStr(std::string("unexpected inst kind: '") +
                              CurTok.str().str() + "'");
          return false;
        }
      }

      if (IK == Inst::Var && InstWidth == 0) {
        ErrStr = makeErrStr(TP, "var must have a width");
        return false;
      }

      if (!consumeToken(ErrStr)) return false;

      Block *B = 0;

      if (IK == Inst::Var || IK == Inst::ReservedConst) {
        llvm::APInt Zero(InstWidth, 0, false), One(InstWidth, 0, false),
                    ConstOne(InstWidth, 1, false), Lower(InstWidth, 0, false),
                    Upper(InstWidth, 0, false);
        llvm::ConstantRange Range(InstWidth, /*isFullSet*/true);
        bool NonZero = false, NonNegative = false, PowOfTwo = false, Negative = false,
          hasExternalUses = false;
        unsigned SignBits = 0;
        while (CurTok.K != Token::ValName && CurTok.K != Token::Ident && CurTok.K != Token::Eof) {
          if (CurTok.K == Token::OpenParen) {
            if (!consumeToken(ErrStr))
              return false;
            switch (CurTok.K) {
              case Token::KnownBits:
                if (InstWidth != CurTok.PatternString.length()) {
                  ErrStr = makeErrStr(TP, "knownbits pattern must be of same length as var width");
                  return false;
                }
                for (unsigned i = 0; i < InstWidth; ++i) {
                  if (CurTok.PatternString[i] == '0')
                    Zero += ConstOne.shl(CurTok.PatternString.length() - 1 - i);
                  else if (CurTok.PatternString[i] == '1')
                    One += ConstOne.shl(CurTok.PatternString.length() - 1 - i);
                  else if (CurTok.PatternString[i] != 'x') {
                    ErrStr = makeErrStr(TP, "invalid knownBits string");
                    return false;
                  }
                }
                if (!consumeToken(ErrStr))
                  return false;
                break;
              case Token::Ident:
                if (CurTok.str() == "powerOfTwo") {
                  PowOfTwo = true;
                  if (!consumeToken(ErrStr))
                    return false;
                } else if (CurTok.str() == "negative") {
                  Negative = true;
                  if (!consumeToken(ErrStr))
                    return false;
                } else if (CurTok.str() == "nonNegative") {
                  NonNegative = true;
                  if (!consumeToken(ErrStr))
                    return false;
                } else if (CurTok.str() == "nonZero") {
                  NonZero = true;
                  if (!consumeToken(ErrStr))
                    return false;
                } else if (CurTok.str() == "signBits") {
                  if (!consumeToken(ErrStr))
                    return false;
                  if (CurTok.K != Token::Eq) {
                    ErrStr = makeErrStr(TP, "expected '=' for number of signBits");
                    return false;
                  }
                  if (!consumeToken(ErrStr))
                    return false;
                  if (CurTok.K != Token::UntypedInt) {
                    ErrStr = makeErrStr(TP, "expected positive integer value for number of sign bits");
                    return false;
                  }
                  SignBits = CurTok.Val.getLimitedValue();
                  if (SignBits == 0) {
                    ErrStr = makeErrStr(TP, "expected positive integer value for number of sign bits");
                    return false;
                  }
                  if (SignBits > InstWidth) {
                    ErrStr = makeErrStr(TP, "number of sign bits can't exceed instruction width and expects positive integer value");
                    return false;
                  }
                  if (!consumeToken(ErrStr))
                    return false;
                } else if (CurTok.str() == "range") {
                  if (!consumeToken(ErrStr))
                    return false;
                  if (CurTok.K != Token::Eq) {
                    ErrStr = makeErrStr(TP, "expected '=' for range as 'range='");
                    return false;
                  }

                  if (!consumeToken(ErrStr))
                    return false;
                  if (CurTok.K != Token::OpenBracket) {
                    ErrStr = makeErrStr(TP, "expected '[' to specify lower bound of range");
                    return false;
                  }

                  if (!consumeToken(ErrStr))
                    return false;
                  if (CurTok.K != Token::UntypedInt) {
                    ErrStr = makeErrStr(TP, "expected lower bound of range");
                    return false;
                  }
                  Lower = CurTok.Val;

                  if (!Lower.isSignedIntN(InstWidth) && (Lower.isNegative() ||
                      (!Lower.isNegative() && !Lower.isIntN(InstWidth)))) {
                    ErrStr = makeErrStr(TP, "Lower bound is out of range");
                    return false;
                  }
                  if (Lower.getBitWidth() != InstWidth)
                    Lower = Lower.sextOrTrunc(InstWidth);

                  if (!consumeToken(ErrStr))
                    return false;
                  if (CurTok.K != Token::Comma) {
                    ErrStr = makeErrStr(TP, "expected ',' after lower bound of range");
                    return false;
                  }

                  if (!consumeToken(ErrStr))
                    return false;
                  if (CurTok.K != Token::UntypedInt) {
                    ErrStr = makeErrStr(TP, "expected upper bound of range");
                    return false;
                  }
                  Upper = CurTok.Val;

                  if (!Upper.isSignedIntN(InstWidth) && (Upper.isNegative() ||
                      (!Upper.isNegative() && !Upper.isIntN(InstWidth)))) {
                    ErrStr = makeErrStr(TP, "Upper bound is out of range");
                    return false;
                  }
                  if (Upper.getBitWidth() != InstWidth)
                    Upper = Upper.sextOrTrunc(InstWidth);

                  if (Lower == Upper && !Lower.isMinValue() && !Lower.isMaxValue()) {
                    ErrStr = makeErrStr(TP, "range with lower == upper is invalid unless it is empty or full set");
                    return false;
                  }

                  if (!consumeToken(ErrStr))
                    return false;
                  if (CurTok.K != Token::CloseParen) {
                    ErrStr = makeErrStr(TP, "expected ')' after upper bound of range");
                    return false;
                  }
                  if (!consumeToken(ErrStr))
                    return false;
                  Range = llvm::ConstantRange(Lower, Upper);
                } else {
                  ErrStr = makeErrStr(TP, "invalid data flow fact type");
                  return false;
                }
                break;
              default:
                ErrStr = makeErrStr(TP, "invalid data flow fact type");
                return false;
                break;
            }
            if (CurTok.K != Token::CloseParen) {
              ErrStr = makeErrStr(TP, "expected ')' to complete data flow fact");
              return false;
            }
            if (!consumeToken(ErrStr))
              return false;
          }
        }
        Inst *I;
        if (IK == Inst::Var)
          I = IC.createVar(InstWidth, InstName, Range, Zero, One, NonZero,
                           NonNegative, PowOfTwo, Negative, SignBits, 0);
        else if (IK == Inst::ReservedConst)
          I = IC.createVar(InstWidth, InstName, Range, Zero, One, NonZero,
                           NonNegative, PowOfTwo, Negative, SignBits,
                           ++ReservedConstCounter);

        Context.setInst(InstName, I);
        return true;
      }

      if (IK == Inst::Phi) {
        if (CurTok.K != Token::ValName) {
          ErrStr = makeErrStr("expected block number");
          return false;
        }
        if (CurTok.Width != 0) {
          ErrStr = makeErrStr("blocks may not have a width");
          return false;
        }
        if (!(B = Context.getBlock(CurTok.Name))) {
          ErrStr =
              makeErrStr(std::string("%") + InstName.str() + " is not a block");
          return false;
        }
        if (!consumeToken(ErrStr)) return false;

        if (CurTok.K != Token::Comma) {
          ErrStr = makeErrStr("expected ','");
          return false;
        }
        if (!consumeToken(ErrStr)) return false;
      } else if (IK == Inst::Hole) {
        Inst *I = IC.createHole(InstWidth);
        Context.setInst(InstName, I);
        return true;
      }

      std::vector<Inst *> Ops;
      bool hasExternalUses = false;

      while (1) {
        Inst *I = parseInst(ErrStr);
        if (!I)
          return false;

        Ops.push_back(I);

        if (CurTok.K != Token::Comma) {
          if (CurTok.K == Token::OpenParen) {
            if (!consumeToken(ErrStr))
              return false;
            if (CurTok.K != Token::Ident || CurTok.str() != "hasExternalUses") {
              ErrStr = makeErrStr(TP, "expected hasExternalUses token");
              return false;
            }
            if (!consumeToken(ErrStr))
              return false;
            if (CurTok.K != Token::CloseParen) {
              ErrStr = makeErrStr(TP, "expected ')' to complete external uses string");
              return false;
            }
            if (!consumeToken(ErrStr))
              return false;
            hasExternalUses = true;
          }
          break;
        }
        if (!consumeToken(ErrStr)) return false;
      }

      Inst *I;
      if (IK == Inst::Phi) {
        assert(B);
        if (!typeCheckPhi(InstWidth, B, Ops, ErrStr)) {
          ErrStr = makeErrStr(TP, ErrStr);
          return false;
        }
        I = IC.getPhi(B, Ops);
      } else {
        if (!typeCheckInst(IK, InstWidth, Ops, ErrStr)) {
          ErrStr = makeErrStr(TP, ErrStr);
          return false;
        }
        switch (IK) {
          case Inst::SAddWithOverflow:
            I = IC.getInst(IK, InstWidth, {IC.getInst(Inst::Add, Ops[0]->Width, Ops),
                           IC.getInst(Inst::SAddO, 1, Ops)});
            break;
          case Inst::UAddWithOverflow:
            I = IC.getInst(IK, InstWidth, {IC.getInst(Inst::Add, Ops[0]->Width, Ops),
                           IC.getInst(Inst::UAddO, 1, Ops)});
            break;
          case Inst::SSubWithOverflow:
            I = IC.getInst(IK, InstWidth, {IC.getInst(Inst::Sub, Ops[0]->Width, Ops),
                           IC.getInst(Inst::SSubO, 1, Ops)});
            break;
          case Inst::USubWithOverflow:
            I = IC.getInst(IK, InstWidth, {IC.getInst(Inst::Sub, Ops[0]->Width, Ops),
                           IC.getInst(Inst::USubO, 1, Ops)});
            break;
          case Inst::SMulWithOverflow:
            I = IC.getInst(IK, InstWidth, {IC.getInst(Inst::Mul, Ops[0]->Width, Ops),
                           IC.getInst(Inst::SMulO, 1, Ops)});
            break;
          case Inst::UMulWithOverflow:
            I = IC.getInst(IK, InstWidth, {IC.getInst(Inst::Mul, Ops[0]->Width, Ops),
                           IC.getInst(Inst::UMulO, 1, Ops)});
            break;
          default:
            I = IC.getInst(IK, InstWidth, Ops);
            break;
        }
      }

      if (hasExternalUses)
        ExternalUsesSet.insert(I);
      for (auto EU: ExternalUsesSet)
        I->DepsWithExternalUses.insert(EU);
      Context.setInst(InstName, I);
      return true;
    }

    default:
      ErrStr =
        makeErrStr("expected inst, block, cand, infer, result, pc, or blockpc");
      return false;
  }
}

ParsedReplacement Parser::parseReplacement (std::string &ErrStr) {
  if (!consumeToken(ErrStr))
    return ParsedReplacement();

  while (CurTok.K != Token::Eof) {
    if (!parseLine(ErrStr))
      return ParsedReplacement();

    if (!Reps.empty()) {
      if (CurTok.K != Token::Eof) {
        ErrStr = makeErrStr("expected a single replacement");
        return ParsedReplacement();
      }
      return std::move(Reps[0]);
    }
  }

  if (RK == ReplacementKind::ParseLHS)
    ErrStr = makeErrStr("incomplete replacement, need an 'infer' statement");
  else if (RK == ReplacementKind::ParseRHS)
    ErrStr = makeErrStr("incomplete replacement, need a 'result' statement");
  else
    ErrStr = makeErrStr(
        "incomplete replacement, need a 'cand' statement or 'infer'/'result' pair");
  return ParsedReplacement();
}

ParsedReplacement souper::ParseReplacement(InstContext &IC,
                                           llvm::StringRef Filename,
                                           llvm::StringRef Str,
                                           std::string &ErrStr) {
  std::vector<ParsedReplacement> Reps;
  Parser P(Filename, Str, IC, Reps, ReplacementKind::ParseBoth, 0, 0);
  ParsedReplacement R = P.parseReplacement(ErrStr);
  if (ErrStr == "") {
    assert(R.Mapping.LHS);
    assert(R.Mapping.RHS);
  }
  return R;
}

ParsedReplacement souper::ParseReplacementLHS(InstContext &IC,
                                              llvm::StringRef Filename,
                                              llvm::StringRef Str,
                                              ReplacementContext &RC,
                                              std::string &ErrStr) {
  std::vector<ParsedReplacement> Reps;
  std::vector<ReplacementContext> RCs;
  Parser P(Filename, Str, IC, Reps, ReplacementKind::ParseLHS, 0, &RCs);
  ParsedReplacement R = P.parseReplacement(ErrStr);
  if (ErrStr == "") {
    assert(R.Mapping.LHS);
    assert(!R.Mapping.RHS);
    assert(RCs.size() == 1);
    RC = RCs[0];
  }
  return R;
}

ParsedReplacement souper::ParseReplacementRHS(InstContext &IC,
                                              llvm::StringRef Filename,
                                              llvm::StringRef Str,
                                              ReplacementContext &RC,
                                              std::string &ErrStr) {
  std::vector<ParsedReplacement> Reps;
  std::vector<ReplacementContext> RCs =  { RC };
  RCs.emplace_back();
  Parser P(Filename, Str, IC, Reps, ReplacementKind::ParseRHS, &RCs, 0);
  ParsedReplacement R = P.parseReplacement(ErrStr);
  if (ErrStr == "") {
    assert(!R.Mapping.LHS);
    assert(R.Mapping.RHS);
  }
  return R;
}

std::vector<ParsedReplacement> Parser::parseReplacements(std::string &ErrStr) {
  if (!consumeToken(ErrStr))
    return Reps;

  while (CurTok.K != Token::Eof) {
    if (!parseLine(ErrStr))
      return Reps;
  }

  if (!PCs.empty() || !BPCs.empty() || !Context.empty() ||
      !BlockPCIdxMap.empty()) {
    ErrStr = makeErrStr("incomplete replacement");
    return Reps;
  }

  return Reps;
}

std::vector<ParsedReplacement> souper::ParseReplacements(
    InstContext &IC, llvm::StringRef Filename, llvm::StringRef Str,
    std::string &ErrStr) {
  std::vector<ParsedReplacement> Reps;
  Parser P(Filename, Str, IC, Reps, ReplacementKind::ParseBoth, 0, 0);
  std::vector<ParsedReplacement> R = P.parseReplacements(ErrStr);
  if (ErrStr == "") {
    for (auto i = R.begin(); i != R.end(); ++i) {
      assert(i->Mapping.LHS);
      assert(i->Mapping.RHS);
    }
  }
  return R;
}

std::vector<ParsedReplacement> souper::ParseReplacementLHSs(
    InstContext &IC, llvm::StringRef Filename, llvm::StringRef Str,
    std::vector<ReplacementContext> &RCs, std::string &ErrStr) {
  assert(RCs.size() == 0);
  std::vector<ParsedReplacement> Reps;
  Parser P(Filename, Str, IC, Reps, ReplacementKind::ParseLHS, 0, &RCs);
  std::vector<ParsedReplacement> R = P.parseReplacements(ErrStr);
  if (ErrStr == "") {
    assert(RCs.size() == R.size());
    for (auto i = R.begin(); i != R.end(); ++i) {
      assert(i->Mapping.LHS);
      assert(!i->Mapping.RHS);
    }
  }
  return R;
}

std::vector<ParsedReplacement> souper::ParseReplacementRHSs(
    InstContext &IC, llvm::StringRef Filename, llvm::StringRef Str,
    std::vector<ReplacementContext> &RCs, std::string &ErrStr) {
  RCs.emplace_back();
  std::vector<ParsedReplacement> Reps;
  Parser P(Filename, Str, IC, Reps, ReplacementKind::ParseRHS, &RCs, 0);
  std::vector<ParsedReplacement> R = P.parseReplacements(ErrStr);
  RCs.pop_back();
  if (ErrStr == "") {
    assert(RCs.size() == R.size());
    for (auto i = R.begin(); i != R.end(); ++i) {
      assert(!i->Mapping.LHS);
      assert(i->Mapping.RHS);
    }
  }
  return R;
}
