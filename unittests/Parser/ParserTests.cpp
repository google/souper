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

#include "llvm/Support/raw_ostream.h"
#include "souper/Parser/Parser.h"
#include "gtest/gtest.h"

using namespace souper;

TEST(ParserTest, Errors) {
  struct {
    std::string Test, WantError;
  } Tests[] = {
      // lexing
      { "%=", "<input>:1:2: expected identifier" },
      { "%0:j", "<input>:1:4: expected 'i'" },
      { "%0:ix", "<input>:1:5: expected integer" },
      { "%0:i0", "<input>:1:5: width must be at least 1" },
      { "0:j", "<input>:1:3: expected 'i'" },
      { "0:ix", "<input>:1:4: expected integer" },
      { "0:i0", "<input>:1:4: width must be at least 1" },
      { "?", "<input>:1:1: unexpected '?'" },
      { "%0:i32 = var-check", "<input>:1:14: unexpected character following a negative sign" },
      { "%0:i32 = add -2, -a", "<input>:1:19: unexpected character following a negative sign" },
      { "%0:i32 = -", "<input>:1:11: unexpected character following a negative sign" },

      // parsing
      { "%2 = block 111111111111111111111\n",
        "<input>:1:12: 4294967295 is too many block predecessors" },
      { "pc 55555550:i5\n",
        "<input>:1:15: integer constant is too large for its width" },
      { "%0:i32 = var\n%1:i1 = eq %0:i32, %0:i32\n",
        "<input>:2:12: inst reference may not have a width" },
      { "%0:i1 = eq %1, %1\n", "<input>:1:12: %1 is not an inst" },
      { "%0:i1 = eq foo\n", "<input>:1:12: unexpected token: 'foo'" },
      { "%0:i1 = var\n%0:i1 = var\n",
        "<input>:2:1: %0 already declared as an inst" },
      { "%0 = block 33\n%0:i1 = var\n",
        "<input>:2:1: %0 already declared as a block" },
      { "%0 foo\n", "<input>:1:4: expected '='" },
      { "%0 = %1\n", "<input>:1:6: expected identifier" },
      { "%0:i1 = block 3\n", "<input>:1:1: blocks may not have a width" },
      { "%0:i1 = foo\n", "<input>:1:9: unexpected inst kind: 'foo'" },
      { "%0 = var\n", "<input>:1:1: var must have a width" },
      { "%0:i1 = phi foo\n", "<input>:1:13: expected block number" },
      { "%0:i1 = block 122\n", "<input>:1:1: blocks may not have a width" },
      { "%0:i1 = phi %0\n", "<input>:1:13: %0 is not a block" },
      { "%0 = block 1\n%1:i1 = phi %0 foo\n", "<input>:2:16: expected ','" },
      { "%0 = block 1\n%1:i32 = var\nblockpc 0 %1",
        "<input>:3:9: expected block var"},
      { "%0 = block 1\n%1:i32 = var\nblockpc %0:i32 0 %2 1",
        "<input>:3:9: blocks may not have a width" },
      { "%0 = block 1\n%1:i32 = var\nblockpc %0 %1\n%2:i32 = var",
        "<input>:3:12: expected block number"},
      { "%0 = block 1\n%1:i32 = var\nblockpc %1 0 %1 1",
        "<input>:3:9: %1 is declared as an inst" },
      { "%0 = block 1\n%1:i32 = var\nblockpc %2 0 %1 1",
        "<input>:3:9: block %2 is undeclared" },
      { ",\n", "<input>:1:1: expected inst, block, cand, infer, result, pc, "
        "or blockpc" },
      { "%0:i40 = var ; 0\n%1:i40 = bswap %0\n",
        "<input>:2:1: bswap argument must be a multiple of 16 bits" },
      { "%0:i4 = var (knownBits=00010)\n",
        "<input>:1:1: knownbits pattern must be of same length as var width" },
      { "%0:i4 = var (knownBits=1xx00)\n",
        "<input>:1:1: knownbits pattern must be of same length as var width" },
      { "%0:i4 = var (knownBits=xx1)\n",
        "<input>:1:1: knownbits pattern must be of same length as var width" },
      { "%0:i4 = var (knownBits=0012)\n",
        "<input>:1:1: knownbits pattern must be of same length as var width" },
      { "%0:i4 = var (knownBits=2345)\n",
        "<input>:1:24: expected [0|1|x]+ for knownBits" },
      { "%0:i4 = var (knownBits=01xa)\n",
        "<input>:1:1: knownbits pattern must be of same length as var width" },
      { "%0:i4 = var (knownBit=xxxx)\n",
        "<input>:1:1: invalid data flow fact type" },
      { "%0:i4 = var ()\n",
        "<input>:1:1: invalid data flow fact type" },
      { "%0:i4 = var (knownBits=10x0\n",
        "<input>:1:1: expected ')' to complete data flow fact" },
      { "%0:i4 = var (knownBits=10\nx0)\n",
        "<input>:1:1: knownbits pattern must be of same length as var width" },
      { "%0:i4 = var (knownBits10x0)\n",
        "<input>:1:23: expected '=' for knownBits" },
      { "%0:i128 = var (knownBits=11111111111111000000000000000111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)",
        "<input>:1:1: knownbits pattern must be of same length as var width" },
      { "%1:i128 = var (knownBits=000000000000000000000000000000000000000000000000000000000�000000000000000000000000000000000000000000000000000000000000000000000)",
        "<input>:1:1: knownbits pattern must be of same length as var width" },
      { "%0:i33 = ssub.with.overflow 0, 1\n"
        "%1:i1 = extractvalue %0, 1:i32\n"
        "infer %1\n",
        "<input>:1:1: at least one operand must be typed" },
      { "%0:i65 = var ; 0\n%1:i1 = extractvalue %0, 1\n"
        "%2:i64 = extractvalue %0, 0\n"
        "%3:i64 = select %1, 18446744073709551615:i64, %2\n"
        "infer %3\n",
        "<input>:2:1: extract value expects an aggregate type" },
      { "%0:i8 = var\n"
        "%1:i8 = var\n"
         "%2:i9 = smul.with.overflow %0, %1\n"
        "%3 = extractvalue %2, 999999999999999999999999999999999999999999999999999999999999999999999999999",
      "<input>:4:1: extractvalue inst doesn't expect index value other than 0 or 1" },
      { "%0:i65 = var ; 0\n"
        "%1:i64 = extractvalue %0, 0\n"
        "infer %1\n",
        "<input>:2:1: extract value expects an aggregate type" },
      { "%0:i65 = var ; 0\n"
        "%1:i1 = extractvalue %0, 1\n"
        "infer %1\n",
        "<input>:2:1: extract value expects an aggregate type" },
      { "%0:i64 = var ; 0\n"
        "%1:i1 = extractvalue 42, 1\n"
        "infer %1\n",
        "<input>:2:1: extract value expects an aggregate type" },
      { "%0:i4 = var (knownBits=001x) (nonZero\n",
        "<input>:1:1: expected ')' to complete data flow fact" },
      { "%0:i4 = var (nonZero\n",
        "<input>:1:1: expected ')' to complete data flow fact" },
      { "%0:i4 = var (nonNegative (nonZero)\n",
        "<input>:1:1: expected ')' to complete data flow fact" },
      { "%0:i4 = var (knownBits=0012) (nonZero)\n",
        "<input>:1:1: knownbits pattern must be of same length as var width" },
      { "%0:i4 = var (knownBits=0011 (nonZero) (nonNegative)\n",
        "<input>:1:1: expected ')' to complete data flow fact" },
      { "%0:i4 = var (a)\n",
        "<input>:1:1: invalid data flow fact type" },
      { "%0:i4 = var (powerOf2)\n",
        "<input>:1:1: invalid data flow fact type" },
      { "%0:i8 = var (powerOfTwo (nonZero)\n",
        "<input>:1:1: expected ')' to complete data flow fact" },
      { "%0:i4 = var (negative\n",
        "<input>:1:1: expected ')' to complete data flow fact" },
      { "%0:i8 = var (signBits)\n",
        "<input>:1:1: expected '=' for number of signBits" },
      { "%0:i8 = var (signBits=-1)\n",
        "<input>:1:1: number of sign bits can't exceed instruction width and expects positive integer value" },
      { "%0:i8 = var (signBits=t)\n",
        "<input>:1:1: expected positive integer value for number of sign bits" },
      { "%0:i8 = var (signBits=0)\n",
        "<input>:1:1: expected positive integer value for number of sign bits" },
      { "%0:i8 = var (signBits=2\n",
        "<input>:1:1: expected ')' to complete data flow fact" },
      { "%0:i8 = var (signBits=9)\n",
        "<input>:1:1: number of sign bits can't exceed instruction width and expects positive integer value" },
      { "%0:i8 = var (knownBits=0001xxxx) (signBit=3)\n",
        "<input>:1:1: invalid data flow fact type" },
      { "%0:i8 = var (signBits=3) (nonNegative) (nonZero) (powerOfTwo\n",
        "<input>:1:1: expected ')' to complete data flow fact" },
      { "%0:i8 = var (nonNegative) (nonZero) (powerOfTwo) (signBits=3\n",
        "<input>:1:1: expected ')' to complete data flow fact" },
      { "%0:i64 = var ; 0\n"
        "%1:i1 = add 42:i32, %0 (^",
        "<input>:2:25: unexpected '^'" },
      { "%0:i64 = var ; 0\n"
        "%1:i1 = extractvalue 42, 1 (hasExternalUses\n"
        "infer %1\n",
        "<input>:2:1: expected ')' to complete external uses string" },
      { "%0:i64 = var ; 0\n"
        "%1:i1 = extractvalue 42, 1 (hasSomethingElse\n"
        "infer %1\n",
        "<input>:2:1: expected hasExternalUses token" },
      { "%0:i8 = var (range[2,90))\n",
        "<input>:1:1: expected '=' for range as 'range='" },
      { "%0:i32 = var (range=2,90))\n",
        "<input>:1:1: expected '[' to specify lower bound of range" },
      { "%0:i32 = var (range=[2:i32,90:i32))\n",
        "<input>:1:1: expected lower bound of range" },
      { "%0:i32 = var (range=[2,90:i32))\n",
        "<input>:1:1: expected upper bound of range" },
      { "%0:i32 = var (range=[2 90))\n",
        "<input>:1:1: expected ',' after lower bound of range" },
      { "%0:i32 = var (range=[2,90)\n",
        "<input>:1:1: expected ')' to complete data flow fact" },
      { "%0:i8 = var (range=[2,256))\n",
        "<input>:1:1: Upper bound is out of range" },
      { "%0:i8 = var (range=[-129,127))\n",
        "<input>:1:1: Lower bound is out of range" },
      { "%0:i8 = var (range=[6,6))\n",
        "<input>:1:1: range with lower == upper is invalid unless it is empty or full set" },

      // type checking
      { "%0 = add 1:i32\n",
        "<input>:1:1: expected 2 operands, found 1" },
      { "%0 = add 1:i32, 2:i32, 3:i32\n",
        "<input>:1:1: expected 2 operands, found 3" },
      { "%0:i64 = add 1:i32, 2:i32\n",
        "<input>:1:1: inst must have width of 32, has width 64" },
      { "%0 = add 1:i32, 2:i64\n",
        "<input>:1:1: operands have different widths" },

      { "%0 = addnsw 1:i32\n",
        "<input>:1:1: expected 2 operands, found 1" },
      { "%0 = addnsw 1:i32, 2:i32, 3:i32\n",
        "<input>:1:1: expected 2 operands, found 3" },
      { "%0:i64 = addnsw 1:i32, 2:i32\n",
        "<input>:1:1: inst must have width of 32, has width 64" },
      { "%0 = addnsw 1:i32, 2:i64\n",
        "<input>:1:1: operands have different widths" },

      { "%0 = eq 1:i32\n",
        "<input>:1:1: expected 2 operands, found 1" },
      { "%0 = eq 1:i32, 2:i32, 3:i32\n",
        "<input>:1:1: expected 2 operands, found 3" },
      { "%0:i64 = eq 1:i32, 2:i32\n",
        "<input>:1:1: inst must have width of 1, has width 64" },
      { "%0 = eq 1:i32, 2:i64\n",
        "<input>:1:1: operands have different widths" },

      { "%0 = select 1:i1, 2:i32\n",
        "<input>:1:1: expected 3 operands, found 2" },
      { "%0 = select 1:i1, 2:i32, 3:i32, 4:i32\n",
        "<input>:1:1: expected 3 operands, found 4" },
      { "%0 = select 1:i32, 2:i32, 3:i32\n",
        "<input>:1:1: first operand must have width of 1, has width 32" },
      { "%0:i64 = select 1:i1, 2:i32, 3:i32\n",
        "<input>:1:1: inst must have width of 32, has width 64" },
      { "%0 = select 1:i1, 2:i32, 3:i64\n",
        "<input>:1:1: operands have different widths" },

      { "%0 = fshl\n",
        "<input>:2:1: unexpected token: ''" },
      { "%0 = fshl 1:i1\n",
        "<input>:1:1: expected 3 operands, found 1" },
      { "%0 = fshl 1:i1, 2:i1\n",
        "<input>:1:1: expected 3 operands, found 2" },
      { "%0 = fshl 1:i2, 2:i1, 3:i1\n",
       "<input>:1:1: operands have different widths" },
      { "%0 = fshl 1:i1, 2:i2, 3:i1\n",
       "<input>:1:1: operands have different widths" },
      { "%0 = fshl 1:i1, 2:i1, 3:i2\n",
       "<input>:1:1: operands have different widths" },
      { "%0 = fshl 1:i2, 2:i2, 3:i1\n",
       "<input>:1:1: operands have different widths" },
      { "%0 = fshl 1:i2, 2:i1, 3:i2\n",
       "<input>:1:1: operands have different widths" },
      { "%0 = fshl 1:i1, 2:i2, 3:i2\n",
       "<input>:1:1: operands have different widths" },
      { "%0 = fshl 1:i1, 2:i1, 3:i1, 4:i1\n",
        "<input>:1:1: expected 3 operands, found 4" },

      { "%0 = fshr\n",
        "<input>:2:1: unexpected token: ''" },
      { "%0 = fshr 1:i1\n",
        "<input>:1:1: expected 3 operands, found 1" },
      { "%0 = fshr 1:i1, 2:i1\n",
        "<input>:1:1: expected 3 operands, found 2" },
      { "%0 = fshr 1:i2, 2:i1, 3:i1\n",
       "<input>:1:1: operands have different widths" },
      { "%0 = fshr 1:i1, 2:i2, 3:i1\n",
       "<input>:1:1: operands have different widths" },
      { "%0 = fshr 1:i1, 2:i1, 3:i2\n",
       "<input>:1:1: operands have different widths" },
      { "%0 = fshr 1:i2, 2:i2, 3:i1\n",
       "<input>:1:1: operands have different widths" },
      { "%0 = fshr 1:i2, 2:i1, 3:i2\n",
       "<input>:1:1: operands have different widths" },
      { "%0 = fshr 1:i1, 2:i2, 3:i2\n",
       "<input>:1:1: operands have different widths" },
      { "%0 = fshr 1:i1, 2:i1, 3:i1, 4:i1\n",
        "<input>:1:1: expected 3 operands, found 4" },

      { "%0 = zext 1:i1\n",
        "<input>:1:1: inst must have a width" },
      { "%0:i33 = zext 1:i1, 2:i32\n",
        "<input>:1:1: expected 1 operands, found 2" },
      { "%0:i32 = zext 1:i64\n",
        "<input>:1:1: inst must have width of at least 65, has width 32" },

      { "%0 = trunc 1:i1\n",
        "<input>:1:1: inst must have a width" },
      { "%0:i33 = trunc 1:i1, 2:i32\n",
        "<input>:1:1: expected 1 operands, found 2" },
      { "%0:i64 = trunc 1:i32\n",
        "<input>:1:1: inst must have width of at most 31, has width 64" },

      { "%0 = block 2\n%1 = phi %0, 1:i32, 2:i32\n%2 = phi %0, 3:i32\n",
        "<input>:3:1: phi has 1 operand(s) but preceding block has 2" },
      { "%0 = block 2\n%1 = phi %0, 1:i32, 2:i64\n",
        "<input>:2:1: operands have different widths" },
      { "%0 = block 2\n%1:i32 = phi %0, 1:i64, 2:i64\n",
        "<input>:2:1: inst must have width of 64, has width 32" },
      { "%0 = block 2\n%1:i32 = var\nblockpc %0 0 %1 1\nblockpc %0 1 %1 1\n"
        "blockpc %0 2 %1 1\n%2:i32 = phi %0, %1, %1",
        "<input>:6:1: blockpc's predecessor number is larger "
        "than the number of phi's operands" },
    };

  InstContext IC;
  for (const auto &T : Tests) {
    std::string ErrStr;
    ParseReplacement(IC, "<input>", T.Test, ErrStr);
    EXPECT_EQ(T.WantError, ErrStr);
  }
  for (const auto &T : Tests) {
    std::string ErrStr;
    ReplacementContext Context;
    ParseReplacementLHS(IC, "<input>", T.Test, Context, ErrStr);
    EXPECT_EQ(T.WantError, ErrStr);
  }
  for (const auto &T : Tests) {
    std::string ErrStr;
    ReplacementContext Context;
    ParseReplacementRHS(IC, "<input>", T.Test, Context, ErrStr);
    EXPECT_EQ(T.WantError, ErrStr);
  }
}

TEST(ParserTest, FullReplacementErrors) {
  struct {
    std::string Test, WantError;
  } Tests[] = {
      // lexing
      { "cand 0:i1 0:i1 ; this is a comment\n", "" },
      { "infer 0:i1\nresult 0:i1 ; this is a comment\n", "<input>:1:7: unexpected infer operand type" },

      // parsing
      { "infer 0:i1\ninfer 0:i1", "<input>:1:7: unexpected infer operand type" },
      { "infer 11111111", "<input>:1:7: unexpected infer operand type" },
      { "infer %0\ninfer %0", "<input>:1:7: %0 is not an inst" },
      { "%0:i32 = var\n%1:i32 = var\ninfer %0 %1", "<input>:3:10: %1 already declared as an inst" },
      { "cand 0:i1 ; this is a comment\n", "<input>:2:1: unexpected token: ''" },
      { "%0:i1 = var\n",
        "<input>:2:1: incomplete replacement, need a 'cand' statement or 'infer'/'result' pair" },
      { "cand 0:i1 0:i1\ncand 0:i1 0:i1\n",
        "<input>:2:1: expected a single replacement" },
      { "%0:i4 = var ; 0\n%1:i4 = mulnuw %0, 1:i4\ninfer %1 ()\n",
        "<input>:3:11: missing Inst attribute string" },
      { "%0:i4 = var ; 0\n%1:i4 = mulnuw %0, 1:i4\ninfer %1 (addd)\n",
        "<input>:3:11: invalid Inst attribute string" },
      { "%0:i4 = var ; 0\ninfer %0 (dBits=0111)\n",
        "<input>:2:11: invalid Inst attribute string" },
      { "%0:i4 = var ; 0\ninfer %0 (0011)\n",
        "<input>:2:11: missing Inst attribute string" },
      { "%0:i4 = var ; 0\ninfer %0 (demandedBits0010)\n",
        "<input>:2:23: expected '=' for demandedBits" },
      { "%0:i4 = var ; 0\ninfer %0 (demandedBits=0010\n",
        "<input>:3:1: expected ')' to complete demandedBits" },
      { "%0:i4 = var ; 0\ninfer %0 (demandedBits=000)\n",
        "<input>:2:24: demandedBits pattern must be of same length as infer operand width" },
      { "%0:i4 = var ; 0\ninfer %0 (demandedBits=11111)\n",
        "<input>:2:24: demandedBits pattern must be of same length as infer operand width" },
      { "%0:i4 = var ; 0\ninfer %0 (demandedBits=nnnn)\n",
        "<input>:2:24: expected demandedBits pattern of type [0|1]+" },
      { "%0:i4 = var ; 0\ninfer %0 (demandedBits=2100)\n",
        "<input>:2:24: expected demandedBits pattern of type [0|1]+" },
      { "%0:i4 = var (knownBits=xxx0) (demandedBits=1111); 0\ninfer %0\n",
        "<input>:1:1: invalid data flow fact type" },
      { "%0:i4 = var ; 0\ninfer %0 (knownBits=0xxx)\n",
        "<input>:2:11: missing Inst attribute string" },
      { "%0:i4 = var ; 0\ncand %0 7:i4 (demandedBits=011)\n",
        "<input>:2:28: demandedBits pattern must be of same length as infer operand width" },
      { "%0:i4 = var ; 0\ncand %0 7:i4 (0111)\n",
        "<input>:2:15: missing Inst attribute string" },
      { "%0:i4 = var ; 0\ncand %0 7:i4 (demandedBit=0111)\n",
        "<input>:2:15: invalid Inst attribute string" },
      { "%0:i4 = var ; 0\ncand %0 7:i4 (demandedBits0111)\n",
        "<input>:2:27: expected '=' for demandedBits" },
      { "%0:i4 = var ; 0\ncand %0 7:i4 (demandedBits=0111\n",
        "<input>:3:1: expected ')' to complete demandedBits" },
      { "%0:i4 = var ; 0\ncand %0 7:i4 (demandedBits=2111)\n",
        "<input>:2:28: expected demandedBits pattern of type [0|1]+" },
      { "%0:i4 = var ; 0\ninfer %0 (harvestedFromUse\n",
        "<input>:3:1: expected ')' to complete harvestFromUse" },
      { "%0:i4 = var ; 0\ninfer %0 (demandedBits=1111) (harvestedFromUse\n",
        "<input>:3:1: expected ')' to complete harvestFromUse" },
      { "%0:i4 = var ; 0\ninfer %0 (demandedBits=1111 (harvestedFromUse\n",
        "<input>:2:29: expected ')' to complete demandedBits" },
      { "%0:i4 = var ; 0\ninfer %0 (harvestedFromUse (demandedBits=1111\n",
        "<input>:2:28: expected ')' to complete harvestFromUse" },
      { "%0:i4 = var ; 0\ninfer %0 (harvestedFromUse (demandedBits=1111\n",
        "<input>:2:28: expected ')' to complete harvestFromUse" },
      { "%0:i4 = var ; 0\ninfer %0 (harvestedFromUse) (harvestedFromUse)\n",
        "<input>:2:30: only one harvestFromUse attribute is allowed" },
      { "%0:i4 = var ; 0\ninfer %0 (demandedBits=1111) (demandedBits=1111)\n",
        "<input>:2:31: only one demandedbits attribute is allowed" },
      { "%0:i4 = var ; 0\ninfer %0 (harvestedFromUse) (harvestedFromUse) (demandedBits=1111)\n",
        "<input>:2:30: only one harvestFromUse attribute is allowed" },
      { "%0:i4 = var ; 0\ninfer %0 (harvestedFromUse) (demandedBits=1111) (harvestedFromUse)\n",
        "<input>:2:50: only one harvestFromUse attribute is allowed" },
      { "%0:i4 = var ; 0\ninfer %0 (demandedBits=1111) (harvestedFromUse) (harvestedFromUse)\n",
        "<input>:2:50: only one harvestFromUse attribute is allowed" },
      { "%0:i4 = var ; 0\ninfer %0 (demandedBits=1111) (demandedBits=1111) (harvestedFromUse)\n",
        "<input>:2:31: only one demandedbits attribute is allowed" },
      { "%0:i4 = var ; 0\ninfer %0 (demandedBits=1111) (harvestedFromUse) (demandedBits=1111)\n",
        "<input>:2:50: only one demandedbits attribute is allowed" },
      { "%0:i4 = var ; 0\ninfer %0 (harvestedFromUse) (demandedBits=1111) (demandedBits=1111)\n",
        "<input>:2:50: only one demandedbits attribute is allowed" },
      { "%0:i4 = var ; 0\ninfer %0 (harvestedFromUse) (demandedBits=1111) (demandedBits=1100)\n",
        "<input>:2:50: only one demandedbits attribute is allowed" },
      { "%0:i64 = var ; 0\n"
        "%1 = and %0, 1\n"
        "%2 = sadd.with.overflow %0, 1\n"
        "%3 = extractvalue %2, 1\n"
        "infer %2\n",
        "<input>:6:1: overflow intrinsic cannot be an operand of infer instruction" },
      { "%0:i64 = var ; 0\n"
        "%1 = and %0, 1\n"
        "%2 = sadd.with.overflow %0, 1\n"
        "%3 = extractvalue %2, 1\n"
        "cand %2 1\n",
        "<input>:6:1: overflow intrinsic cannot be an operand of cand instruction" },
      { "%0:i64 = var ; 0\n"
        "%1 = and %0, 1\n"
        "%2 = sadd.with.overflow %0, 1\n"
        "%3 = extractvalue %2, 1\n"
        "cand 1 %2\n",
        "<input>:6:1: overflow intrinsic cannot be an operand of cand instruction" },
      { "%0:i64 = var ; 0\n"
        "%1 = uadd.with.overflow %0, 1\n"
        "pc %1 1\n"
        "infer %1\n",
        "<input>:4:1: overflow intrinsic cannot be an operand of pc instruction" },
      { "%0 = block 2\n"
        "%1:i32 = var ; 0\n"
        "%2 = smul.with.overflow %1, 2\n"
        "%3 = ne 1, %1\n"
        "%4:i33 = zext %3\n"
        "blockpc %0 0 %2 1\n"
        "blockpc %0 1 %4 0\n"
        "%5:i32 = phi %0, 1, 0\n"
        "infer %5\n",
        "<input>:7:1: overflow intrinsic cannot be an operand of blockpc instruction" },
      { "%0:i32 = var ; 0\n"
        "%1:i32 = ashr-1:i32, %0\n"
        "%2:i32 = and %0, %1\n"
        "%3 = ne 1, %1\n"
        "infer %2 (demandedBits=00000000000000000000000000000001)\n"
        "result 32:i332\n",
        "<input>:7:1: width of result and infer operands mismatch" },

      // type checking
      { "%0:i64 = var ; 0\n"
        "%1 = uadd.with.overflow %0, 1\n"
        "%2 = eq %1, 1\n"
        "infer %2\n",
        "<input>:3:1: overflow intrinsic cannot be an operand of eq instruction" },
      { "%0:i64 = var ; 0\n"
        "%1 = uadd.with.overflow %0, 1\n"
        "%2 = mul %1, 10\n"
        "infer %2\n",
        "<input>:3:1: overflow intrinsic cannot be an operand of mul instruction" },
      { "%0 = block 2\n"
        "%1:i32 = var ; 0\n"
        "%2:i32 = var ; 1\n"
        "%3 = smul.with.overflow %1, 2\n"
        "blockpc %0 0 %1 0\n"
        "blockpc %0 1 %1 1\n"
        "%4:i33 = phi %0, %3, 0:i33\n"
        "%5:i16 = trunc %4\n"
        "infer %5\n",
        "<input>:7:1: overflow intrinsic cannot be an operand of phi instruction" },
    };

  InstContext IC;
  for (const auto &T : Tests) {
    std::string ErrStr;
    ParseReplacement(IC, "<input>", T.Test, ErrStr);
    EXPECT_EQ(T.WantError, ErrStr);
  }
}

TEST(ParserTest, ReplacementLHSErrors) {
  struct {
    std::string Test, WantError;
  } Tests[] = {
      // lexing
      { "infer 0:i1 ; this is a comment\n", "<input>:1:7: unexpected infer operand type" },

      // parsing
      { "%0:i1 = var\n",
        "<input>:2:1: incomplete replacement, need an 'infer' statement" },
      { "infer 0:i1 0:i1 ; this is a comment\n", "<input>:1:7: unexpected infer operand type" },
      { "cand 0:i1 0:i1", "<input>:1:1: Not expecting 'cand' when parsing LHS" },
      { "infer 0:i1\ninfer 0:i1", "<input>:1:7: unexpected infer operand type" },

      // type checking
    };

  InstContext IC;
  for (const auto &T : Tests) {
    std::string ErrStr;
    ReplacementContext Context;
    ParseReplacementLHS(IC, "<input>", T.Test, Context, ErrStr);
    EXPECT_EQ(T.WantError, ErrStr);
  }
}

TEST(ParserTest, ReplacementRHSErrors) {
  struct {
    std::string Test, WantError;
  } Tests[] = {
      // lexing
      { "result 0:i1 ; this is a comment\n", "" },

      // parsing
      { "%0:i1 = var\n",
        "<input>:2:1: incomplete replacement, need a 'result' statement" },
      { "infer 0:i1 0:i1 ; this is a comment\n",
        "<input>:1:1: Not expecting 'infer' when parsing RHS" },
      { "cand 0:i1 0:i1", "<input>:1:1: Not expecting 'cand' when parsing RHS" },
      { "result 0:i1\nresult 0:i1", "<input>:2:1: expected a single replacement" },
      { "result 32", "<input>:1:8: expecting width to be specified for int operand in 'result' instruction" },

      // type checking
    };

  InstContext IC;
  for (const auto &T : Tests) {
    std::string ErrStr;
    ReplacementContext Context;
    ParseReplacementRHS(IC, "<input>", T.Test, Context, ErrStr);
    EXPECT_EQ(T.WantError, ErrStr);
  }
}

TEST(ParserTest, RoundTrip) {
  std::string Tests[] = {
      R"i(%0:i1 = var ; 0
cand %0 0:i1
)i",
      R"i(%0 = block 2
%1:i32 = var ; 1
%2:i32 = lshr %1, 31:i32
%3:i32 = var ; 3
%4:i32 = udiv %2, %3
%5:i1 = eq 0:i32, %3
%6:i32 = zext %5
%7:i32 = phi %0, %4, %6
%8:i32 = ashr %7, 1:i32
%9:i1 = eq 0:i32, %8
cand %9 1:i1
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = ctpop %0
%2:i1 = eq 1:i32, %1
cand %2 1:i1
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = cttz %0
%2:i1 = eq 1:i32, %1
cand %2 1:i1
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = ctlz %0
%2:i1 = eq 1:i32, %1
cand %2 1:i1
)i",
      R"i(%0:i33 = var ; 0
%1:i33 = ctpop %0
%2:i1 = eq 1:i33, %1
cand %2 1:i1
)i",
      R"i(%0:i33 = var ; 0
%1:i33 = cttz %0
%2:i1 = eq 1:i33, %1
cand %2 1:i1
)i",
      R"i(%0:i33 = var ; 0
%1:i33 = ctlz %0
%2:i1 = eq 1:i33, %1
cand %2 1:i1
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = var ; 1
%2:i32 = fshl %0, %1, 0:i32
cand %2 %0
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = var ; 1
%2:i32 = fshl %0, %1, 32:i32
cand %2 %0
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = var ; 1
%2:i32 = fshl %0, %1, 33:i32
%3:i32 = fshl %0, %1, 1:i32
cand %2 %3
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = var ; 1
%2:i32 = fshr %0, %1, 0:i32
cand %2 %1
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = var ; 1
%2:i32 = fshr %0, %1, 32:i32
cand %2 %1
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = var ; 1
%2:i32 = fshr %0, %1, 33:i32
%3:i32 = fshr %0, %1, 1:i32
cand %2 %3
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = bswap %0
%2:i1 = eq 1:i32, %1
cand %2 1:i1
)i",
      R"i(%0:i128 = var ; 0
%1:i128 = bswap %0
%2:i1 = eq 1:i128, %1
cand %2 1:i1
)i",
      R"i(%0:i35 = var ; 0
%1:i35 = bitreverse %0
%2:i1 = eq 1:i35, %1
cand %2 0:i1
)i",
      R"i(%0:i64 = var ; 0
%1:i64 = xor 18446744073709551615:i64, %0
%2:i64 = add 1:i64, %1
%3:i64 = sub 0:i64, %0
cand %2 %3
)i",
      R"i(%0 = block 1
%1:i32 = var ; 1
blockpc %0 1 %1 1:i32
blockpc %0 0 %1 2:i32
%2:i32 = var ; 2
cand %1 %2
)i",
      R"i(%0:i32 = var ; 0
%1:i1 = ult 0:i32, %0
%2:i33 = usub.with.overflow 0:i32, %0
%3:i1 = extractvalue %2, 1:i32
%4:i1 = xor %1, %3
cand %4 0:i1
)i",
      R"i(%0:i32 = var ; 0
%1:i33 = uadd.with.overflow %0, 1:i32
%2:i32 = extractvalue %1, 0:i32
%3:i1 = ult 0:i32, %2
%4:i1 = extractvalue %1, 1:i32
%5:i1 = or %3, %4
cand %5 1:i1
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = shl %0, 3:i32
%2:i64 = sext %1
%3:i64 = var ; 3
%4:i64 = sext %0
%5:i65 = sadd.with.overflow %3, %4
%6:i64 = extractvalue %5, 0:i32
%7:i65 = ssub.with.overflow %6, 1:i64
%8:i64 = extractvalue %7, 0:i32
%9:i65 = sadd.with.overflow %2, %8
%10:i64 = extractvalue %9, 0:i32
%11:i64 = srem %10, %2
%12:i65 = ssub.with.overflow %2, %11
%13:i1 = extractvalue %12, 1:i32
cand %13 0:i1
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = add 0:i32, %0 (hasExternalUses)
%2:i32 = add 0:i32, %1 (hasExternalUses)
%3:i32 = add 0:i32, %2 (hasExternalUses)
%4:i32 = add 0:i32, %3
cand %4 %0
)i",
      R"i(%0:i1 = var ; 0
%1:i1 = add 0:i1, %0 (hasExternalUses)
%2:i1 = add %0, %1
cand %2 1:i1
)i",
      R"i(%0:i8 = var (range=[1,3)) ; 0
%1:i8 = var (range=[1,5)) ; 1
%2:i8 = shl %0, %1
%3:i1 = ule %2, 32:i8
cand %3 1:i1
)i",
      R"i(%0:i32 = var (range=[10,9)) ; 0
%1:i1 = eq 9:i32, %0
cand %1 0:i1
)i",
      R"i(%0:i32 = var ; 0
%1:i32 = shl %0, 3:i32
%2:i64 = sext %1
%3:i64 = var ; 3
%4:i64 = sext %0
%5:i65 = sadd.with.overflow %3, %4 (hasExternalUses)
%6:i64 = extractvalue %5, 0:i32 (hasExternalUses)
%7:i65 = ssub.with.overflow %6, 1:i64
%8:i64 = extractvalue %7, 0:i32 (hasExternalUses)
%9:i65 = sadd.with.overflow %2, %8
%10:i64 = extractvalue %9, 0:i32
%11:i64 = srem %10, %2 (hasExternalUses)
%12:i65 = ssub.with.overflow %2, %11
%13:i1 = extractvalue %12, 1:i32 (hasExternalUses)
cand %13 0:i1
)i",
      R"i(%0:i32 = var (range=[10,9)) ; 0
%1:i1 = eq 9:i32, %0
cand %1 0:i1 (harvestedFromUse)
)i",
      R"i(%0:i32 = var (range=[10,9)) ; 0
%1:i1 = eq 9:i32, %0
cand %1 0:i1 (demandedBits=0) (harvestedFromUse)
)i",
  };

  struct {
    std::string Test, Want;
  } NonEqualTests[] = {
    {R"i(%foo = add 1, 2:i32
cand %foo 3:i32
)i",
     R"i(%0:i32 = add 1:i32, 2:i32
cand %0 3:i32
)i"},
    {R"i(%foo = add 1:i32, 2
cand %foo 3
)i",
     R"i(%0:i32 = add 1:i32, 2:i32
cand %0 3:i32
)i"},
    {R"i(%foo = select 1, 2:i32, 3
cand %foo 3
)i",
     R"i(%0:i32 = select 1:i1, 2:i32, 3:i32
cand %0 3:i32
)i"},
    {R"i(%0:i8 = var
%1:i1 = eq 1, %0
%2:i1 = eq 2, %0
%3:i1 = or %1, %2
%4:i1 = eq 4, %0
%5:i1 = eq 8, %0
%6:i1 = or %4, %5
%7:i1 = or %3, %6
%8:i1 = eq 16, %0
%9:i1 = eq 32, %0
%10:i1 = or %8, %9
%11:i1 = eq 64, %0
%12:i1 = eq 128, %0
%13:i1 = or %11, %12
%14:i1 = or %10, %13
%15:i1 = or %7, %14
%16:i1 = eq 1:i1, %15
cand %15 %16
)i",
     R"i(%0:i8 = var
%1:i1 = eq 1:i8, %0
%2:i1 = eq 2:i8, %0
%3:i1 = or %1, %2
%4:i1 = eq 4:i8, %0
%5:i1 = eq 8:i8, %0
%6:i1 = or %4, %5
%7:i1 = or %3, %6
%8:i1 = eq 16:i8, %0
%9:i1 = eq 32:i8, %0
%10:i1 = or %8, %9
%11:i1 = eq 64:i8, %0
%12:i1 = eq 128:i8, %0
%13:i1 = or %11, %12
%14:i1 = or %10, %13
%15:i1 = or %7, %14
%16:i1 = eq 1:i1, %15
cand %15 %16
)i" },
    {R"i(%0 = block 1
%1 = block 2
%2:i32 = var
blockpc %0 0 %2 1
blockpc %1 0 %2 2
blockpc %0 1 %2 3
%3:i32 = var
cand %2 %3
)i",
     R"i(%0 = block 1
%1:i32 = var
blockpc %0 0 %1 1:i32
%2 = block 2
blockpc %2 0 %1 2:i32
blockpc %0 1 %1 3:i32
%3:i32 = var
cand %1 %3
)i" },
    {R"i(%0:i32 = var (range=[10,9))
%1:i1 = eq 9:i32, %0
cand %1 0:i1 (harvestedFromUse) (demandedBits=0)
)i",
     R"i(%0:i32 = var (range=[10,9))
%1:i1 = eq 9:i32, %0
cand %1 0:i1 (demandedBits=0) (harvestedFromUse)
)i" },
  };

  InstContext IC;
  for (const auto &T : Tests) {
    std::string ErrStr;
    auto R = ParseReplacement(IC, "<input>", T, ErrStr);
    ASSERT_EQ("", ErrStr);
    EXPECT_EQ(R.getString(/*printNames=*/true), T);

    ReplacementContext Context1, Context2, Context3;
    auto LHS = R.getLHSString(Context1);
    auto R2 = ParseReplacementLHS(IC, "<input>", LHS, Context2, ErrStr);
    ASSERT_EQ("", ErrStr);
    auto LHS2 = R2.getLHSString(Context3);
    EXPECT_EQ(LHS, LHS2);

    auto RHS = R.getRHSString(Context1);
    auto R3 = ParseReplacementRHS(IC, "<input>", RHS, Context2, ErrStr);
    ASSERT_EQ("", ErrStr);
    auto RHS2 = R3.getRHSString(Context3);
    EXPECT_EQ(RHS, RHS2);

    auto Split = LHS + RHS;
    auto R4 = ParseReplacement(IC, "<input>", Split, ErrStr);
    ASSERT_EQ("", ErrStr);
    EXPECT_EQ(R4.getString(/*printNames=*/true), T);
  }

  for (const auto &T : NonEqualTests) {
    std::string ErrStr;
    auto R = ParseReplacement(IC, "<input>", T.Test, ErrStr);
    ASSERT_EQ("", ErrStr);
    EXPECT_EQ(R.getString(), T.Want);

    ReplacementContext Context1, Context2, Context3;
    auto LHS = R.getLHSString(Context1);
    auto R2 = ParseReplacementLHS(IC, "<input>", LHS, Context2, ErrStr);
    ASSERT_EQ("", ErrStr);
    auto LHS2 = R2.getLHSString(Context3);
    EXPECT_EQ(LHS, LHS2);

    auto RHS = R.getRHSString(Context1);
    auto R3 = ParseReplacementRHS(IC, "<input>", RHS, Context2, ErrStr);
    ASSERT_EQ("", ErrStr);
    auto RHS2 = R3.getRHSString(Context3);
    EXPECT_EQ(RHS, RHS2);

    auto Split = LHS + RHS;
    auto R4 = ParseReplacement(IC, "<input>", Split, ErrStr);
    ASSERT_EQ("", ErrStr);
    EXPECT_EQ(R4.getString(), T.Want);
  }
}

int countSubstring(const std::string& str, const std::string& sub)
{
  if (sub.length() == 0)
    return 0;
  int count = 0;
  for (size_t offset = str.find(sub); offset != std::string::npos;
       offset = str.find(sub, offset + sub.length()))
    ++count;
  return count;
}

TEST(ParserTest, RoundTripMultiple) {
  struct {
    std::string Test;
    int N;
  } Tests[] = {
      { R"i(%0 = block 2
%1 = block 2
%2:i56 = var ; 2
%3:i56 = and 8998403161718784:i56, %2
%4:i1 = ne 0:i56, %3
%5:i32 = zext %4
%6:i32 = var ; 6
%7:i32 = shl %6, 24:i32
%8:i32 = ashrexact %7, 24:i32
%9:i32 = sdiv %5, %8
%10:i32 = phi %1, %9, %5
%11:i16 = trunc %10
%12:i16 = shl %11, 8:i16
%13:i16 = ashrexact %12, 8:i16
%14 = block 3
%15:i32 = var ; 15
%16:i32 = var ; 16
%17:i32 = phi %14, 1:i32, %15, %16
%18:i16 = trunc %17
%19:i16 = add %13, %18
%20:i1 = ne 65535:i16, %19
%21:i32 = zext %20
%22:i16 = var ; 22
%23:i32 = sext %22
%24:i32 = sdiv %21, %23
%25:i32 = phi %0, %24, %21
%26:i32 = add 250:i32, %25
%27:i32 = and 255:i32, %26
%28:i1 = ne 0:i32, %27
cand %28 1:i1

%0:i32 = var ; 0
%1:i56 = var ; 1
%2:i56 = shl %1, 7:i56
%3:i56 = ashr %2, 41:i56
%4:i32 = trunc %3
%5:i32 = xor %0, %4
%6:i32 = var ; 6
%7:i1 = eq 4294938068:i32, %6
%8:i32 = zext %7
%9:i1 = sle %5, %8
%10:i16 = zext %9
%11:i16 = or 142:i16, %10
%12:i16 = mul 65492:i16, %11
%13:i1 = eq 0:i16, %12
cand %13 0:i1

%0:i64 = var ; 0
%1:i1 = eq 0:i64, %0
%2:i32 = zext %1
%3:i32 = lshr 5:i32, %2
%4:i8 = trunc %3
%5:i1 = eq 0:i8, %4
cand %5 0:i1

%0:i8 = var ; 0
%1:i32 = sext %0
%2:i1 = slt %0, 0:i8
%3:i32 = select %2, 0:i32, 6:i32
%4:i32 = ashr %1, %3
%5:i1 = ult 31:i32, %4
%6:i32 = lshr 32767:i32, %4
%7:i1 = slt %6, 4:i32
%8:i1 = or %5, %7
%9:i32 = shl 4:i32, %4
%10:i8 = trunc %9
%11:i8 = select %8, 4:i8, %10
%12:i32 = var ; 12
%13:i1 = ult 31:i32, %12
%14:i32 = lshr 1:i32, %12
%15:i8 = trunc %14
%16:i8 = xor 40:i8, %15
%17:i8 = select %13, 41:i8, %16
%18:i8 = sub %11, %17
%19:i1 = slt %18, 0:i8
cand %19 1:i1

%0:i64 = var ; 0
%1:i1 = eq 0:i64, %0
%2:i32 = zext %1
%3:i32 = lshr 5:i32, %2
%4:i8 = trunc %3
%5:i1 = eq 0:i8, %4
cand %5 0:i1

%0:i8 = var ; 0
%1:i32 = sext %0
%2:i1 = slt %0, 0:i8
%3:i32 = select %2, 0:i32, 6:i32
%4:i32 = ashr %1, %3
%5:i1 = ult 31:i32, %4
%6:i32 = lshr 32767:i32, %4
%7:i1 = slt %6, 4:i32
%8:i1 = or %5, %7
cand %8 0:i1

%0:i8 = var ; 0
%1:i32 = sext %0
%2:i1 = slt %0, 0:i8
%3:i32 = select %2, 0:i32, 6:i32
%4:i32 = ashr %1, %3
%5:i32 = lshr 32767:i32, %4
%6:i1 = slt %5, 4:i32
cand %6 0:i1

)i", 7 },
      { R"i(%0:i64 = var ; 0
%1:i64 = xor 18446744073709551615:i64, %0
%2:i64 = add 1:i64, %1
%3:i64 = sub 0:i64, %0
cand %2 %3

%0:i32 = var ; 0
%1:i32 = var ; 1
%2:i32 = xor 4294967295:i32, %1
%3:i32 = or %0, %2
%4:i32 = xor 4294967295:i32, %3
%5:i32 = xor 4294967295:i32, %0
%6:i32 = and %1, %5
cand %4 %6

%0:i16 = var ; 0
%1:i16 = var ; 1
%2:i16 = xor 65535:i16, %1
%3:i16 = and %0, %2
%4:i16 = xor 65535:i16, %3
%5:i16 = xor 65535:i16, %0
%6:i16 = or %1, %5
cand %4 %6

%0:i64 = var ; 0
%1:i64 = var ; 1
%2:i64 = and %0, %1
%3:i64 = var ; 3
%4:i64 = and %1, %3
%5:i64 = or %2, %4
%6:i64 = or %0, %3
%7:i64 = and %1, %6
cand %5 %7

%0:i64 = var ; 0
%1:i64 = var ; 1
%2:i64 = sub %0, %1
%3:i64 = add %0, %2
%4:i64 = sub %1, %0
%5:i64 = add %4, %4
%6:i64 = sub %1, %5
cand %3 %6

%0:i64 = var ; 0
%1:i64 = var ; 1
%2:i64 = var ; 2
%3:i64 = xor %1, %2
%4:i64 = and %0, %3
%5:i64 = and %0, %1
%6:i64 = and %0, %2
%7:i64 = xor %5, %6
cand %4 %7

)i", 6 },
  };

  InstContext IC;
  for (const auto &T : Tests) {
    std::string ErrStr;
    auto R = ParseReplacements(IC, "<input>", T.Test, ErrStr);
    EXPECT_EQ("", ErrStr);
    EXPECT_EQ(T.N, R.size());

    std::string LHSStr, RHSStr;
    for (auto i = R.begin(); i != R.end(); ++i) {
      ReplacementContext Context;
      LHSStr += i->getLHSString(Context) + '\n';
      RHSStr += i->getRHSString(Context) + '\n';
    }
    EXPECT_EQ(T.N, countSubstring(LHSStr, "infer"));
    EXPECT_EQ(T.N, countSubstring(RHSStr, "result"));
    EXPECT_EQ(0, countSubstring(LHSStr, "result"));
    EXPECT_EQ(0, countSubstring(RHSStr, "infer"));
    std::vector<ReplacementContext> Contexts;
    auto LHSs = ParseReplacementLHSs(IC, "<input>", LHSStr, Contexts, ErrStr);
    ASSERT_EQ("", ErrStr);
    EXPECT_EQ(T.N, LHSs.size());
    auto RHSs = ParseReplacementRHSs(IC, "<input>", RHSStr, Contexts, ErrStr);
    ASSERT_EQ("", ErrStr);
    EXPECT_EQ(T.N, RHSs.size());

    std::string Split;
    for (auto i = LHSs.begin(), j = RHSs.begin(); i != LHSs.end(); ++i, ++j) {
      ReplacementContext Context;
      Split += i->getLHSString(Context);
      Split += j->getRHSString(Context) + '\n';
    }
    // one more RT to get the "cand" instructions back
    auto R2 = ParseReplacements(IC, "<input>", Split, ErrStr);
    ASSERT_EQ("", ErrStr);
    EXPECT_EQ(T.N, R2.size());
    std::string UnSplit;
    for (auto i = R2.begin(); i != R2.end(); ++i) {
      UnSplit += i->getString(/*printNames=*/true) + '\n';
    }
    EXPECT_EQ(T.Test, UnSplit);
  }
}
