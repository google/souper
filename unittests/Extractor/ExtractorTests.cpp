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

#include "llvm/AsmParser/Parser.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/SourceMgr.h"
#include "souper/Extractor/Candidates.h"
#include "souper/Extractor/ExprBuilder.h"
#include <memory>
#include "gtest/gtest.h"

using namespace llvm;
using namespace souper;

unsigned DebugLevel;

LLVMContext C;

struct ExtractorTest : testing::Test {
  std::unique_ptr<Module> M;

  InstContext IC;
  ExprBuilderContext EBC;
  FunctionCandidateSet CS;
  std::vector<std::string> CandExprs;

  bool extractFromIR(const char *IR) {
    SMDiagnostic Err;
    M = parseAssemblyString(IR, Err, C);
    if (!M.get()) {
      Err.print("ExtractorTest", errs());
      return false;
    }

    if (M->size() != 1) {
      EXPECT_EQ(1u, M->size());
      return false;
    }

    ExprBuilderOptions Opts;
    Opts.NamedArrays = true;

    CS = ExtractCandidates(&*M->begin(), IC, EBC, Opts);
    for (auto &B : CS.Blocks) {
      for (auto &R : B->Replacements) {
        if (R.Mapping.LHS->Width > 1)
          continue;
        std::vector<Inst *>Guesses { IC.getConst(APInt(1, false)),
                                     IC.getConst(APInt(1, true)) };
        for (auto I : Guesses) {
          R.Mapping.RHS = I;
          std::unique_ptr<ExprBuilder> EB = createKLEEBuilder(IC);
          std::string Cand = EB->GetExprStr(R.BPCs, R.PCs, R.Mapping, 0);
          CandExprs.emplace_back(Cand);
        }
      }
    }

    return true;
  }

  bool hasCandidate(std::string Expected) {
    for (auto &B : CS.Blocks) {
      for (auto &R : B->Replacements) {
        if (R.Mapping.LHS->Width > 1)
          continue;
        std::string Str;
        llvm::raw_string_ostream SS(Str);
        R.print(SS, /*printNames=*/true);
        if (SS.str() == Expected)
          return true;
      }
    }
    return false;
  }

  bool hasCandidateExpr(std::string Expected) {
    for (const auto &Cand : CandExprs) {
      if (Cand == Expected)
        return true;
    }
    return false;
  }
};

TEST_F(ExtractorTest, Simple) {
  ASSERT_TRUE(extractFromIR(R"m(
define i1 @f(i32 %p, i32 %q) {
  %ult = icmp ult i32 %p, %q

  %add = add i32 %p, %q
  %ult1 = icmp ult i32 %p, %add

  %and = or i1 %ult, %ult1
  ret i1 %and
}
)m"));

  EXPECT_TRUE(hasCandidateExpr("(Ult (Read w32 0 p) (Read w32 0 q))"));
  EXPECT_TRUE(
      hasCandidateExpr("(Eq false (Ult (Read w32 0 p) (Read w32 0 q)))"));

  EXPECT_TRUE(
      hasCandidateExpr("(Ult N0:(Read w32 0 p) (Add w32 N0 (Read w32 0 q)))"));
}

TEST_F(ExtractorTest, Nsw) {
  ASSERT_TRUE(extractFromIR(R"m(
define i1 @f(i32 %p, i32 %q) {
  %add = add nsw i32 %p, %q
  %ult = icmp ult i32 %p, %add
  ret i1 %ult
}
)m"));

  EXPECT_TRUE(hasCandidateExpr(
      "(Or (And (Eq N0:(Extract 0 (AShr w32 N1:(Read w32 0 p) 31)) (Extract 0 "
      "(AShr w32 N2:(Read w32 0 q) 31))) (Eq false (Eq N0 (Extract 0 (AShr w32 "
      "N3:(Add w32 N1 N2) 31))))) (Ult N1 N3))"));
}

TEST_F(ExtractorTest, PhiCond) {
  ASSERT_TRUE(extractFromIR(R"m(
define i1 @f(i32 %p, i32 %q) {
entry:
  br i1 undef, label %t, label %f

t:
  %paq = add i32 %p, %q
  br label %cont

f:
  %pmq = mul i32 %p, %q
  br label %cont

cont:
  %phi = phi i32 [ %paq, %t ], [ %pmq, %f ]
  %ult = icmp ult i32 %p, %phi
  ret i1 %ult
}
)m"));

  EXPECT_TRUE(hasCandidateExpr(
      "(Ult N0:(Read w32 0 p) (Select w32 (Read 0 blockpred) (Add w32 N0 "
      "N1:(Read w32 0 q)) (Mul w32 N0 N1)))"));
}

TEST_F(ExtractorTest, PhiLoop) {
  ASSERT_TRUE(extractFromIR(R"m(
define void @f(i32 %p, i32 %q) {
entry:
  br label %loop

loop:
  %phi = phi i32 [ %phia1, %loop ], [ 0, %entry ]
  %phia1 = add i32 %phi, 1
  %eq = icmp eq i32 %phia1, 42
  br i1 %eq, label %cont, label %loop

cont:
  ret void
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(%0:i32 = var (range=[0,42)) ; phi
%1:i32 = add 1:i32, %0 (hasExternalUses)
%2:i1 = eq 42:i32, %1
cand %2 1:i1
)c")); // %phia1 has external uses.
}

TEST_F(ExtractorTest, PathCondition) {
  // Expected equivalence classes are {p, q, r} and {s}.
  ASSERT_TRUE(extractFromIR(R"m(
define i1 @f(i1 %p, i1 %q, i1 %r, i1 %s) {
entry:
  %pq = and i1 %p, %q
  br i1 %pq, label %bb1, label %u

bb1:
  %qr = and i1 %q, %r
  br i1 %qr, label %u, label %bb2

bb2:
  br i1 %r, label %bb3, label %u

bb3:
  br i1 %s, label %bb4, label %u

bb4:
  %cmp1 = icmp eq i1 %p, true
  %cmp2 = icmp eq i1 %s, true
  %or = or i1 %cmp1, %cmp2
  ret i1 %or

u:
  unreachable
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(%0:i1 = var ; p
%1:i1 = var ; q
%2:i1 = and %0, %1
pc %2 1:i1
%3:i1 = var ; r
%4:i1 = and %1, %3
pc %4 0:i1
pc %3 1:i1
%5:i1 = eq 1:i1, %0
cand %5 1:i1
)c"));

  EXPECT_TRUE(hasCandidate(R"c(%0:i1 = var ; s
pc %0 1:i1
%1:i1 = eq 1:i1, %0
cand %1 1:i1
)c"));
}

TEST_F(ExtractorTest, ExternalUses) {
  // Expected equivalence classes are {p, q, r} and {s}.
  ASSERT_TRUE(extractFromIR(R"m(
@amem = common global i1 0, align 4
@bmem = common global i1 0, align 4
@cmem = common global i1 0, align 4

define i1 @foo(i1 %x) {
entry:
  store i1 %x, i1* @amem, align 4
  %a = add i1 %x, 0
  store i1 %a, i1* @amem, align 4
  %b = add i1 %a, 0
  store i1 %b, i1* @bmem, align 4
  %c = add i1 %b, 0
  store i1 %c, i1* @cmem, align 4
  %d = add i1 %c, 0
  ret i1 %d
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(%0:i1 = var ; x
%1:i1 = add 0:i1, %0 (hasExternalUses)
%2:i1 = add 0:i1, %1 (hasExternalUses)
%3:i1 = add 0:i1, %2 (hasExternalUses)
%4:i1 = add 0:i1, %3
cand %4 1:i1
)c"));
}

TEST_F(ExtractorTest, NoExternalUses) {
  // Expected equivalence classes are {p, q, r} and {s}.
  ASSERT_TRUE(extractFromIR(R"m(
define i1 @foo(i1 %x) {
entry:
  %a = add i1 %x, 0
  %b = add i1 %a, 0
  %c = add i1 %b, 0
  %d = add i1 %c, 0
  ret i1 %d
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(%0:i1 = var ; x
%1:i1 = add 0:i1, %0
%2:i1 = add 0:i1, %1
%3:i1 = add 0:i1, %2
%4:i1 = add 0:i1, %3
cand %4 1:i1
)c"));
}

TEST_F(ExtractorTest, PartialExternalUses) {
  // Expected equivalence classes are {p, q, r} and {s}.
  ASSERT_TRUE(extractFromIR(R"m(
define i1 @foo(i1 %x) {
entry:
  %a = add i1 %x, 0
  %b = add i1 %a, %x
  %c = add i1 %b, %a
  ret i1 %c
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(%0:i1 = var ; x
%1:i1 = add 0:i1, %0
cand %1 1:i1
)c"));

  EXPECT_TRUE(hasCandidate(R"c(%0:i1 = var ; x
%1:i1 = add 0:i1, %0 (hasExternalUses)
%2:i1 = add %0, %1
cand %2 1:i1
)c"));
  EXPECT_TRUE(hasCandidate(R"c(%0:i1 = var ; x
%1:i1 = add 0:i1, %0
%2:i1 = add %0, %1
%3:i1 = add %1, %2
cand %3 1:i1
)c"));
}
