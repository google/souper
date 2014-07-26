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
    InstNum,
    Comma,
    Eq,
    Int,
    Eof,
  };

  Kind K;
  const char *Pos;
  size_t Len;
  APInt Val;
  unsigned Num, Width;

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

  return Token{Token::Eof, Begin};

FoundChar:
  switch (*Begin) {
    case ',':
      ++Begin;
      return Token{Token::Comma, Begin-1, 1};
    case '=':
      ++Begin;
      return Token{Token::Eq, Begin-1, 1};
    case '%': {
      ++Begin;
      unsigned Num = 0;
      unsigned Width = 0;
      const char *NumBegin = Begin;
      while (Begin != End && *Begin >= '0' && *Begin <= '9') {
        Num = Num*10 + (*Begin - '0');
        ++Begin;
      }
      if (Begin == NumBegin) {
        ErrStr = "expected integer";
        return Token{Token::Error, Begin, 0};
      }
      if (Begin != End && *Begin == ':') {
        ++Begin;
        if (Begin == End || *Begin != 'i') {
          ErrStr = "expected 'i'";
          return Token{Token::Error, Begin, 0};
        }

        ++Begin;
        const char *WidthBegin = Begin;
        while (Begin != End && *Begin >= '0' && *Begin <= '9') {
          Width = Width*10 + (*Begin - '0');
          ++Begin;
        }
        if (Begin == WidthBegin) {
          ErrStr = "expected integer";
          return Token{Token::Error, Begin, 0};
        }
      }

      Token T;
      T.K = Token::InstNum;
      T.Pos = NumBegin-1;
      T.Len = Begin - NumBegin + 1;
      T.Num = Num;
      T.Width = Width;
      return T;
    }
  }

  if (*Begin >= 'a' && *Begin <= 'z') {
    const char *TokenBegin = Begin;
    do {
      ++Begin;
    } while (Begin != End && *Begin >= 'a' && *Begin <= 'z');
    return Token{Token::Ident, TokenBegin, size_t(Begin - TokenBegin)};
  }

  if (*Begin == '-' || (*Begin >= '0' && *Begin <= '9')) {
    const char *NumBegin = Begin;
    do {
      ++Begin;
    } while (Begin != End && *Begin >= '0' && *Begin <= '9');
    const char *NumEnd = Begin;
    if (Begin == End || *Begin != ':') {
      ErrStr = "expected ':'";
      return Token{Token::Error, Begin, 0};
    }
    ++Begin;
    if (Begin == End || *Begin != 'i') {
      ErrStr = "expected 'i'";
      return Token{Token::Error, Begin, 0};
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
      return Token{Token::Error, Begin, 0};
    }

    return Token{Token::Int, NumBegin, size_t(Begin - NumBegin),
                 APInt(Width, StringRef(NumBegin, NumEnd - NumBegin), 10)};
  }

  ErrStr = std::string("unexpected '") + *Begin + "'";
  ++Begin;
  return Token{Token::Error, Begin-1, 0};
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
  std::map<unsigned, Inst *> InstMap;
  std::map<unsigned, Block *> BlockMap;

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

  bool parseLine(std::string &ErrStr);
};

}

Inst *Parser::parseInst(std::string &ErrStr) {
  switch (CurTok.K) {
    case Token::InstNum: {
      if (CurTok.Width != 0) {
        ErrStr = makeErrStr("inst reference may not have a width");
        return 0;
      }
      auto InstBlockIt = InstMap.find(CurTok.Num);
      if (InstBlockIt == InstMap.end()) {
        ErrStr = makeErrStr(std::string("%") + llvm::utostr(CurTok.Num) +
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

    default: {
      ErrStr = makeErrStr("unexpected token");
      return 0;
    }
  }
}

InstMapping Parser::parseInstMapping(std::string &ErrStr) {
  Inst *S = parseInst(ErrStr);
  if (!S)
    return InstMapping();

  Inst *R = parseInst(ErrStr);
  if (!R)
    return InstMapping();

  return InstMapping(S, R);
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

    case Token::InstNum: {
      unsigned InstNum = CurTok.Num;
      unsigned InstWidth = CurTok.Width;

      if (InstMap.find(InstNum) != InstMap.end()) {
        ErrStr = makeErrStr(std::string("%") + llvm::utostr(InstNum) +
                            " already declared as an inst");
        return false;
      }
      if (BlockMap.find(InstNum) != BlockMap.end()) {
        ErrStr = makeErrStr(std::string("%") + llvm::utostr(InstNum) +
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
                          .Case("sub", Inst::Sub)
                          .Case("subnsw", Inst::SubNSW)
                          .Case("mul", Inst::Mul)
                          .Case("udiv", Inst::UDiv)
                          .Case("sdiv", Inst::SDiv)
                          .Case("urem", Inst::URem)
                          .Case("srem", Inst::SRem)
                          .Case("and", Inst::And)
                          .Case("or", Inst::Or)
                          .Case("xor", Inst::Xor)
                          .Case("shl", Inst::Shl)
                          .Case("lshr", Inst::LShr)
                          .Case("ashr", Inst::AShr)
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
          BlockMap[InstNum] = 0;
          return consumeToken(ErrStr);
        } else {
          ErrStr = makeErrStr(std::string("unexpected inst kind: '") +
                              CurTok.str().str() + "'");
          return false;
        }
      }

      if (InstWidth == 0) {
        ErrStr = makeErrStr(TP, "insts must have a width");
        return false;
      }

      if (!consumeToken(ErrStr)) return false;

      std::map<unsigned, Block *>::iterator PhiBlockIt;
      if (IK == Inst::Var) {
        InstMap[InstNum] = IC.createVar(InstWidth, "");
        return true;
      }

      if (IK == Inst::Phi) {
        if (CurTok.K != Token::InstNum) {
          ErrStr = makeErrStr("expected block number");
          return false;
        }
        if (CurTok.Width != 0) {
          ErrStr = makeErrStr("blocks may not have a width");
          return false;
        }
        PhiBlockIt = BlockMap.find(CurTok.Num);
        if (PhiBlockIt == BlockMap.end()) {
          ErrStr = makeErrStr(std::string("%") + llvm::utostr(InstNum) +
                              " is not a block");
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
        I = IC.getPhi(PhiBlockIt->second, Ops);
      } else {
        I = IC.getInst(IK, InstWidth, Ops);
      }

      InstMap[InstNum] = I;
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
