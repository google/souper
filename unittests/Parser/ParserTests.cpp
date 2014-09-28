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

      // parsing
      { "%0:i32 = var\n%1:i1 = eq %0:i32, %0:i32\n",
        "<input>:2:12: inst reference may not have a width" },
      { "%0:i1 = eq %1, %1\n", "<input>:1:12: %1 is not an inst" },
      { "%0:i1 = eq foo\n", "<input>:1:12: unexpected token" },
      { "%0:i1 = var\n%0:i1 = var\n",
        "<input>:2:1: %0 already declared as an inst" },
      { "%0 = block\n%0:i1 = var\n",
        "<input>:2:1: %0 already declared as a block" },
      { "%0 foo\n", "<input>:1:4: expected '='" },
      { "%0 = %1\n", "<input>:1:6: expected identifier" },
      { "%0:i1 = block\n", "<input>:1:1: blocks may not have a width" },
      { "%0:i1 = foo\n", "<input>:1:9: unexpected inst kind: 'foo'" },
      { "%0 = var\n", "<input>:1:1: var must have a width" },
      { "%0:i1 = phi foo\n", "<input>:1:13: expected block number" },
      { "%0:i1 = block\n", "<input>:1:1: blocks may not have a width" },
      { "%0:i1 = phi %0\n", "<input>:1:13: %0 is not a block" },
      { "%0 = block\n%1:i1 = phi %0 foo\n", "<input>:2:16: expected ','" },
      { ",\n", "<input>:1:1: expected inst, block, cand, infer, result, or pc" },

      // type checking
      { "%0 = add 1:i32\n",
        "<input>:1:1: expected at least 2 operands, found 1" },
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

      { "%0 = block\n%1 = phi %0, 1:i32, 2:i32\n%2 = phi %0, 3:i32\n",
        "<input>:3:1: number of operands inconsistent between phis" },
      { "%0 = block\n%1 = phi %0, 1:i32, 2:i64\n",
        "<input>:2:1: operands have different widths" },
      { "%0 = block\n%1:i32 = phi %0, 1:i64, 2:i64\n",
        "<input>:2:1: inst must have width of 64, has width 32" },
    };

  InstContext IC;
  for (const auto &T : Tests) {
    std::string ErrStr;
    ParseReplacement(IC, "<input>", T.Test, ErrStr);
    EXPECT_EQ(T.WantError, ErrStr);
  }
  for (const auto &T : Tests) {
    std::string ErrStr;
    ParseReplacementLHS(IC, "<input>", T.Test, ErrStr);
    EXPECT_EQ(T.WantError, ErrStr);
  }
}

TEST(ParserTest, FullReplacementErrors) {
  struct {
    std::string Test, WantError;
  } Tests[] = {
      // lexing
      { "cand 0:i1 0:i1 ; this is a comment\n", "" },
      { "infer 0:i1\nresult 0:i1 ; this is a comment\n", "" },

      // parsing
      { "infer 0:i1\ninfer 0:i1", "<input>:2:7: Not expecting a second 'infer'" },
      { "cand 0:i1 ; this is a comment\n", "<input>:2:1: unexpected token" },
      { "%0:i1 = var\n",
        "<input>:2:1: incomplete replacement, need a 'cand' statement or 'infer'/'result' pair" },
      { "cand 0:i1 0:i1\ncand 0:i1 0:i1\n",
        "<input>:2:1: expected a single replacement" },

      // type checking
    };

  InstContext IC;
  for (const auto &T : Tests) {
    std::string ErrStr;
    ParseReplacement(IC, "<input>", T.Test, ErrStr);
    EXPECT_EQ(T.WantError, ErrStr);
  }
}

TEST(ParserTest, PartialReplacementErrors) {
  struct {
    std::string Test, WantError;
  } Tests[] = {
      // lexing
      { "infer 0:i1 ; this is a comment\n", "" },

      // parsing
      { "%0:i1 = var\n",
        "<input>:2:1: incomplete replacement, need an 'infer' statement" },
      { "infer 0:i1 0:i1 ; this is a comment\n", "<input>:1:12: expected a single replacement" },
      { "cand 0:i1 0:i1", "<input>:1:1: Not expecting 'cand' in a partial replacement" },
      { "infer 0:i1\ninfer 0:i1", "<input>:2:1: expected a single replacement" },

      // type checking
    };

  InstContext IC;
  for (const auto &T : Tests) {
    std::string ErrStr;
    ParseReplacementLHS(IC, "<input>", T.Test, ErrStr);
    EXPECT_EQ(T.WantError, ErrStr);
  }
}

TEST(ParserTest, RoundTrip) {
  std::string Tests[] = {
      "cand 0:i1 0:i1\n",
      R"i(%0 = block
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
  };

  InstContext IC;
  for (const auto &T : Tests) {
    std::string ErrStr;
    auto R = ParseReplacement(IC, "<input>", T, ErrStr);
    ASSERT_EQ("", ErrStr);
    EXPECT_EQ(R.getString(), T);

    auto TPartial = R.getLHSString();
    auto R2 = ParseReplacementLHS(IC, "<input>", TPartial, ErrStr);
    ASSERT_EQ("", ErrStr);
    auto TPartial2 = R2.getLHSString();
    EXPECT_EQ(TPartial, TPartial2);

    auto TSplit = TPartial + R.getRHSString();
    auto R3 = ParseReplacement(IC, "<input>", TSplit, ErrStr);
    ASSERT_EQ("", ErrStr);
    EXPECT_EQ(R3.getString(), T);
  }

  for (const auto &T : NonEqualTests) {
    std::string ErrStr;
    auto R = ParseReplacement(IC, "<input>", T.Test, ErrStr);
    ASSERT_EQ("", ErrStr);
    EXPECT_EQ(R.getString(), T.Want);

    auto TPartial = R.getLHSString();
    auto R2 = ParseReplacementLHS(IC, "<input>", TPartial, ErrStr);
    ASSERT_EQ("", ErrStr);
    auto TPartial2 = R2.getLHSString();
    EXPECT_EQ(TPartial, TPartial2);

    auto TSplit = TPartial + R.getRHSString();
    auto R3 = ParseReplacement(IC, "<input>", TSplit, ErrStr);
    ASSERT_EQ("", ErrStr);
    EXPECT_EQ(R3.getString(), T.Want);
  }
}

TEST(ParserTest, RoundTripMultiple) {
  struct {
    std::string Test;
    int N;
    bool Partial;
  } Tests[] = {
      { R"i(%0:i8 = var ; 0
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
infer %19

%0:i8 = var ; 0
%1:i32 = sext %0
%2:i1 = slt %0, 0:i8
%3:i32 = select %2, 0:i32, 6:i32
%4:i32 = ashr %1, %3
%5:i1 = ult 31:i32, %4
%6:i32 = lshr 32767:i32, %4
%7:i1 = slt %6, 4:i32
%8:i1 = or %5, %7
infer %8

%0:i8 = var ; 0
%1:i32 = sext %0
%2:i1 = slt %0, 0:i8
%3:i32 = select %2, 0:i32, 6:i32
%4:i32 = ashr %1, %3
%5:i32 = lshr 32767:i32, %4
%6:i1 = slt %5, 4:i32
infer %6

)i", 3, true },
      { R"i(%0:i32 = var ; 0
%1:i64 = zext %0
%2:i64 = and 1:i64, %1
%3:i64 = subnsw 0:i64, %2
%4:i8 = trunc %3
%5:i8 = sub 0:i8, %4
%6:i1 = ult %5, 4:i8
cand %6 1:i1

%0:i32 = var ; 0
%1:i8 = var ; 1
%2:i1 = eq 1:i8, %1
%3:i32 = zext %2
%4:i32 = and %0, %3
%5:i32 = zext %1
%6:i32 = shlnw %5, 16:i32
%7:i32 = addnw 65536:i32, %6
%8:i32 = lshrexact %7, 16:i32
%9:i1 = ult %4, %8
cand %9 1:i1

%0:i8 = var ; 0
%1 = block
%2:i32 = var ; 2
%3:i32 = var ; 3
%4 = block
%5:i32 = var ; 5
%6:i1 = ult %5, 2:i32
%7:i64 = zext %6
%8:i64 = var ; 8
%9:i64 = add 1:i64, %8
%10:i1 = ule %7, %9
%11:i1 = phi %4, 1:i1, %10
%12:i32 = zext %11
%13:i1 = eq %3, %12
%14:i32 = zext %13
%15:i32 = xor %2, %14
%16:i32 = phi %1, %15
%17:i8 = trunc %16
%18:i1 = eq %0, %17
pc %18 1:i1
%19:i16 = var ; 19
%20:i32 = var ; 20
%21:i16 = trunc %20
%22:i16 = mul %19, %21
%23:i1 = ne 0:i16, %22
%24:i8 = add 246:i8, %0
%25:i1 = slt 0:i8, %24
%26:i64 = zext %22
%27:i64 = xor 9223372036854775807:i64, %26
%28:i64 = sext %24
%29:i1 = slt %27, %28
%30:i1 = and %23, %25, %29
cand %30 0:i1

%0:i8 = var ; 0
%1 = block
%2:i32 = var ; 2
%3:i32 = var ; 3
%4 = block
%5:i32 = var ; 5
%6:i1 = ult %5, 2:i32
%7:i64 = zext %6
%8:i64 = var ; 8
%9:i64 = add 1:i64, %8
%10:i1 = ule %7, %9
%11:i1 = phi %4, 1:i1, %10
%12:i32 = zext %11
%13:i1 = eq %3, %12
%14:i32 = zext %13
%15:i32 = xor %2, %14
%16:i32 = phi %1, %15
%17:i8 = trunc %16
%18:i1 = eq %0, %17
pc %18 1:i1
%19:i16 = var ; 19
%20:i32 = var ; 20
%21:i16 = trunc %20
%22:i16 = mul %19, %21
%23:i64 = zext %22
%24:i64 = xor 9223372036854775807:i64, %23
%25:i8 = add 246:i8, %0
%26:i64 = sext %25
%27:i1 = slt %24, %26
cand %27 0:i1

)i", 4, false },
  };

  InstContext IC;
  for (const auto &T : Tests) {
    std::string ErrStr;
    std::vector<ParsedReplacement> R;
    if (T.Partial) {
      R = ParseReplacementLHSs(IC, "<input>", T.Test, ErrStr);
    } else {
      R = ParseReplacements(IC, "<input>", T.Test, ErrStr);
    }
    EXPECT_EQ("", ErrStr);
    EXPECT_EQ(T.N, R.size());

    std::string TPartial;
    for (auto i = R.begin(); i != R.end(); ++i) {
      TPartial += i->getLHSString() + '\n';
    }
    auto R2 = ParseReplacementLHSs(IC, "<input>", TPartial, ErrStr);
    ASSERT_EQ("", ErrStr);
    EXPECT_EQ(T.N, R2.size());
    std::string TPartial2;
    for (auto i = R2.begin(); i != R2.end(); ++i) {
      TPartial2 += i->getLHSString() + '\n';
    }
    EXPECT_EQ(TPartial, TPartial2);

    if (T.Partial) {
      EXPECT_EQ(T.Test, TPartial2);
    } else {
      std::string TSplit;
      for (auto i = R.begin(); i != R.end(); ++i) {
        TSplit += i->getLHSString() + i->getRHSString() + '\n';        
      }
      // one more RT to get the "cand" instructions back
      auto R3 = ParseReplacements(IC, "<input>", TSplit, ErrStr);
      ASSERT_EQ("", ErrStr);
      EXPECT_EQ(T.N, R3.size());
      std::string UnSplit;
      for (auto i = R3.begin(); i != R3.end(); ++i) {
        UnSplit += i->getString() + '\n';
      }
      EXPECT_EQ(T.Test, UnSplit);
    }
  }
}
