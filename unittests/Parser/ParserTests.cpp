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
        {"%foo", "<input>:1:2: expected integer"},
        {"%0:j", "<input>:1:4: expected 'i'"},
        {"%0:ix", "<input>:1:5: expected integer"},
        {"0?", "<input>:1:2: expected ':'"},
        {"0:j", "<input>:1:3: expected 'i'"},
        {"0:ix", "<input>:1:4: expected integer"},
        {"?", "<input>:1:1: unexpected '?'"},
        {"cand 0:i1 0:i1 ; this is a comment\n", ""},

        {"%0:i32 = var\n%1:i1 = eq %0:i32, %0:i32\n",
         "<input>:2:12: inst reference may not have a width"},
        {"%0:i1 = eq %1, %1\n", "<input>:1:12: %1 is not an inst"},
        {"%0:i1 = eq foo\n", "<input>:1:12: unexpected token"},
        {"%0:i1 = var\n%0:i1 = var\n",
         "<input>:2:1: %0 already declared as an inst"},
        {"%0 = block\n%0:i1 = var\n",
         "<input>:2:1: %0 already declared as a block"},
        {"%0 foo\n", "<input>:1:4: expected '='"},
        {"%0 = %1\n", "<input>:1:6: expected identifier"},
        {"%0:i1 = block\n", "<input>:1:1: blocks may not have a width"},
        {"%0:i1 = foo\n", "<input>:1:9: unexpected inst kind: 'foo'"},
        {"%0 = var\n", "<input>:1:1: insts must have a width"},
        {"%0:i1 = phi foo\n", "<input>:1:13: expected block number"},
        {"%0:i1 = block\n", "<input>:1:1: blocks may not have a width"},
        {"%0:i1 = phi %0\n", "<input>:1:13: %0 is not a block"},
        {"%0 = block\n%1:i1 = phi %0 foo\n", "<input>:2:16: expected ','"},
        {",\n", "<input>:1:1: expected inst, block, cand or pc"},
        {"cand 0:i1 0:i1\ncand 0:i1 0:i1\n",
         "<input>:2:1: expected a single replacement"},
        {"%0:i1 = var\n",
         "<input>:2:1: incomplete replacement, need a 'cand' statement"},
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
%1:i32 = var
%2:i32 = lshr %1, 31:i32
%3:i32 = var
%4:i32 = udiv %2, %3
%5:i1 = eq 0:i32, %3
%6:i32 = zext %5
%7:i32 = phi %0, %4, %6
%8:i32 = ashr %7, 1:i32
%9:i1 = eq 0:i32, %8
cand %9 1:i1
)i",
  };

  InstContext IC;
  for (const auto &T : Tests) {
    std::string ErrStr;
    auto R = ParseReplacement(IC, "<input>", T, ErrStr);
    EXPECT_EQ("", ErrStr);

    std::string NewStr;
    llvm::raw_string_ostream OS(NewStr);
    R.print(OS);
    EXPECT_EQ(T, OS.str());
  }
}
