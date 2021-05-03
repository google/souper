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
#include "souper/Codegen/Codegen.h"
#include "gtest/gtest.h"

unsigned DebugLevel;

using namespace souper;

const BackendCost C5a { .C = { 1, 2, 3, 4, 5 }};
const BackendCost C5b { .C = { 0, 0, 0, 0, 0 }};
const BackendCost C5c { .C = { 1, 2, 3, 4, 4 }};
const BackendCost C5d { .C = { 1, 2, 3, 4, 6 }};

TEST(CodegenTest, Compare) {
  const struct {
    BackendCost L, R;
  } Tests[] = {
    { C5b, C5a },
    { C5b, C5c },
    { C5b, C5d },
  };
  
  for (const auto &T : Tests) {
    EXPECT_EQ(compareCosts(T.L, T.R), true);
    EXPECT_EQ(compareCosts(T.R, T.L), false);
  }
}

TEST(CodegenTest, Sort) {
  const struct {
    std::vector<BackendCost> Costs;
    BackendCost Best;
  } Tests[] = {
    { { C5a, C5b, C5c, C5d }, C5b },
  };
  
  for (const auto &T : Tests) {
    //std::sort(T.Costs.begin(), T.Costs.end(), compareCosts);
    //EXPECT_EQ(T.WantError, ErrStr);
  }
}
