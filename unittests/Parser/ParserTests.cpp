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
      { "cand 0:i1 0:i1 ; this is a comment\n", "" },

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
      { ",\n", "<input>:1:1: expected inst, block, cand or pc" },
      { "cand 0:i1 0:i1\ncand 0:i1 0:i1\n",
        "<input>:2:1: expected a single replacement" },
      { "%0:i1 = var\n",
        "<input>:2:1: incomplete replacement, need a 'cand' statement" },

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

    std::string NewStr;
    llvm::raw_string_ostream OS(NewStr);
    R.print(OS);
    EXPECT_EQ(OS.str(), T);
  }

  for (const auto &T : NonEqualTests) {
    std::string ErrStr;
    auto R = ParseReplacement(IC, "<input>", T.Test, ErrStr);
    ASSERT_EQ("", ErrStr);

    std::string NewStr;
    llvm::raw_string_ostream OS(NewStr);
    R.print(OS);
    EXPECT_EQ(OS.str(), T.Want);
  }
}
