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
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "souper/Extractor/Candidates.h"
#include "souper/Inst/Inst.h"

#include <string>

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
    Eof,
  };

  Kind K;
  const char *Pos;
  size_t Len;
  APInt Val;
  StringRef Name;
  unsigned Width;

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
                              (*Begin >= 'a' && *Begin <= 'z'))) {
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

  if (*Begin >= 'a' && *Begin <= 'z') {
    const char *TokenBegin = Begin;
    do {
      ++Begin;
    } while (Begin != End && *Begin >= 'a' && *Begin <= 'z');
    return Token{Token::Ident, TokenBegin, size_t(Begin - TokenBegin), APInt()};
  }

  if (*Begin == '-' || (*Begin >= '0' && *Begin <= '9')) {
    const char *NumBegin = Begin;
    do {
      ++Begin;
    } while (Begin != End && *Begin >= '0' && *Begin <= '9');
    const char *NumEnd = Begin;
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
      return Token{Token::Int, NumBegin, size_t(Begin - NumBegin),
                   APInt(Width, StringRef(NumBegin, NumEnd - NumBegin), 10)};
    }

    return Token{Token::UntypedInt, NumBegin, size_t(Begin - NumBegin),
                 APInt((NumEnd - NumBegin) * 4,
                        StringRef(NumBegin, NumEnd - NumBegin), 10)};
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

struct Parser {
  Parser(StringRef FileName, StringRef Str, InstContext &IC,
         std::vector<ParsedReplacement> &Reps)
      : FileName(FileName),
        L(Str.data(), Str.data() + Str.size()),
        IC(IC),
        Reps(Reps) {}

  std::string FileName;
  Lexer L;
  Token CurTok;
  InstContext &IC;
  std::vector<ParsedReplacement> &Reps;

  std::vector<InstMapping> PCs;
  std::map<StringRef, Inst *> InstMap;
  std::map<StringRef, Block *> BlockMap;

  std::string makeErrStr(const std::string &ErrStr) {
    return makeErrStr(L.getTokenPos(CurTok), ErrStr);
  }

  std::string makeErrStr(TokenPos TP, const std::string &ErrStr) {
    unsigned Line, Col;
    return FileName + ":" + utostr(TP.Line) + ":" + utostr(TP.Col) + ": " +
           ErrStr;
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
};

}

Inst *Parser::parseInst(std::string &ErrStr) {
  switch (CurTok.K) {
    case Token::ValName: {
      if (CurTok.Width != 0) {
        ErrStr = makeErrStr("inst reference may not have a width");
        return 0;
      }
      auto InstBlockIt = InstMap.find(CurTok.Name);
      if (InstBlockIt == InstMap.end()) {
        ErrStr = makeErrStr(std::string("%") + CurTok.Name.str() +
                            " is not an inst");
        return 0;
      }
      if (!consumeToken(ErrStr))
        return 0;
      return InstBlockIt->second;
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
      ErrStr = makeErrStr("unexpected token");
      return 0;
    }
  }
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
      Op = IC.getConst(Op->Val.sextOrTrunc(Width));
    }
  }
  return true;
}

bool Parser::typeCheckPhi(unsigned Width, Block *B,
                          std::vector<Inst *> &Ops, std::string &ErrStr) {
  if (B->Preds != Ops.size()) {
    ErrStr = "number of operands inconsistent between phis";
    return false;
  }

  if (!typeCheckOpsMatchingWidths(Ops, ErrStr))
    return false;

  if (Width != 0 && Width != Ops[0]->Width) {
    ErrStr = "inst must have width of " + utostr(Ops[0]->Width) +
                         ", has width " + utostr(Width);
    return false;
  }

  return true;
}

bool Parser::typeCheckInst(Inst::Kind IK, unsigned &Width,
                           std::vector<Inst *> &Ops,
                           std::string &ErrStr) {
  unsigned MinOps = 2, MaxOps = ~0U;
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

  case Inst::Select:
    MinOps = MaxOps = 3;
    if (Ops.size() > 1) {
      if (Ops[0]->Width == 0) {
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

  if (!typeCheckOpsMatchingWidths(OpsMatchingWidths, ErrStr))
    return false;

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

  return true;
}

InstMapping Parser::parseInstMapping(std::string &ErrStr) {
  Inst *SrcRep[2];
  SrcRep[0] = parseInst(ErrStr);
  if (!SrcRep[0])
    return InstMapping();

  SrcRep[1] = parseInst(ErrStr);
  if (!SrcRep[1])
    return InstMapping();

  if (!typeCheckOpsMatchingWidths(SrcRep, ErrStr))
    return InstMapping();

  return InstMapping(SrcRep[0], SrcRep[1]);
}

bool Parser::parseLine(std::string &ErrStr) {
  switch (CurTok.K) {
    case Token::Ident:
      if (CurTok.str() == "cand") {
        if (!consumeToken(ErrStr)) return false;
        InstMapping Cand = parseInstMapping(ErrStr);
        if (!ErrStr.empty()) return false;

        Reps.push_back(ParsedReplacement{Cand, std::move(PCs)});
        PCs.clear();
        InstMap.clear();
        BlockMap.clear();

        return true;
      } else if (CurTok.str() == "pc") {
        if (!consumeToken(ErrStr)) return false;
        InstMapping PC = parseInstMapping(ErrStr);
        if (!ErrStr.empty()) return false;

        PCs.push_back(PC);

        return true;
      } else {
        ErrStr = makeErrStr(std::string("unexpected identifier: '") +
                            CurTok.str().str() + "'");
        return false;
      }

    case Token::ValName: {
      StringRef InstName = CurTok.Name;
      unsigned InstWidth = CurTok.Width;

      if (InstMap.find(InstName) != InstMap.end()) {
        ErrStr = makeErrStr(std::string("%") + InstName.str() +
                            " already declared as an inst");
        return false;
      }
      if (BlockMap.find(InstName) != BlockMap.end()) {
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

      Inst::Kind IK = StringSwitch<Inst::Kind>(CurTok.str())
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
                          .Default(Inst::Kind(~0));

      if (IK == Inst::Kind(~0)) {
        if (CurTok.str() == "block") {
          if (InstWidth != 0) {
            ErrStr = makeErrStr(TP, "blocks may not have a width");
            return false;
          }

          // We create the block later, as we need to know how many predecessors
          // it has.
          BlockMap[InstName] = 0;
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

      std::map<StringRef, Block *>::iterator PhiBlockIt;
      if (IK == Inst::Var) {
        InstMap[InstName] = IC.createVar(InstWidth, InstName);
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
        PhiBlockIt = BlockMap.find(CurTok.Name);
        if (PhiBlockIt == BlockMap.end()) {
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
      }

      std::vector<Inst *> Ops;

      while (1) {
        Inst *I = parseInst(ErrStr);
        if (!I)
          return false;

        Ops.push_back(I);

        if (CurTok.K != Token::Comma) break;
        if (!consumeToken(ErrStr)) return false;
      }

      Inst *I;
      if (IK == Inst::Phi) {
        if (!PhiBlockIt->second) {
          PhiBlockIt->second = IC.createBlock(Ops.size());
        }
        if (!typeCheckPhi(InstWidth, PhiBlockIt->second, Ops, ErrStr)) {
          ErrStr = makeErrStr(TP, ErrStr);
          return false;
        }
        I = IC.getPhi(PhiBlockIt->second, Ops);
      } else {
        if (!typeCheckInst(IK, InstWidth, Ops, ErrStr)) {
          ErrStr = makeErrStr(TP, ErrStr);
          return false;
        }
        I = IC.getInst(IK, InstWidth, Ops);
      }

      InstMap[InstName] = I;
      return true;
    }

    default:
      ErrStr = makeErrStr("expected inst, block, cand or pc");
      return false;
  }
}

ParsedReplacement souper::ParseReplacement(InstContext &IC,
                                           llvm::StringRef Filename,
                                           llvm::StringRef Str,
                                           std::string &ErrStr) {
  std::vector<ParsedReplacement> Reps;

  Parser P(Filename, Str, IC, Reps);
  if (!P.consumeToken(ErrStr))
    return ParsedReplacement();

  while (P.CurTok.K != Token::Eof) {
    if (!P.parseLine(ErrStr))
      return ParsedReplacement();

    if (!Reps.empty()) {
      if (P.CurTok.K != Token::Eof) {
        ErrStr = P.makeErrStr("expected a single replacement");
        return ParsedReplacement();
      }
      return std::move(Reps[0]);
    }
  }

  ErrStr = P.makeErrStr("incomplete replacement, need a 'cand' statement");
  return ParsedReplacement();
}

std::vector<ParsedReplacement> souper::ParseReplacements(
    InstContext &IC, llvm::StringRef Filename, llvm::StringRef Str,
    std::string &ErrStr) {
  std::vector<ParsedReplacement> Reps;

  Parser P(Filename, Str, IC, Reps);
  if (!P.consumeToken(ErrStr))
    return Reps;

  while (P.CurTok.K != Token::Eof) {
    if (!P.parseLine(ErrStr))
      return Reps;
  }

  if (!P.PCs.empty() || !P.InstMap.empty() || !P.BlockMap.empty()) {
    ErrStr = P.makeErrStr("incomplete replacement");
    return Reps;
  }

  return Reps;
}
