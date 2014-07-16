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

#include "klee/Expr.h"
#include "klee/util/ExprPPrinter.h"
#include "klee/util/PrintContext.h"
#include "llvm/AsmParser/Parser.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/SourceMgr.h"
#include "souper/Extractor/Candidates.h"
#include "souper/Extractor/KLEEBuilder.h"
#include <memory>
#include "gtest/gtest.h"

using namespace klee;
using namespace llvm;
using namespace souper;

struct ExtractorTest : testing::Test {
  std::unique_ptr<Module> M;

  InstContext IC;
  ExprBuilderContext EBC;
  FunctionCandidateSet CS;
  std::vector<std::unique_ptr<CandidateExpr>> CandExprs;

  bool extractFromIR(const char *IR) {
    LLVMContext &C = getGlobalContext();
    SMDiagnostic Err;
    M.reset(ParseAssemblyString(IR, 0, Err, C));
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
        std::unique_ptr<CandidateExpr> CE(
            new CandidateExpr(GetCandidateExprForReplacement(R.PCs, R.Mapping)));
        if (!IsTriviallyInvalid(CE->E)) {
          CandExprs.emplace_back(std::move(CE));
        }
      }
    }

    return true;
  }

  bool hasCandidate(std::string Expected) {
#if 0
    /* If there is no expected candidate, for the cases where
       bitcode can't be optimized, simply return */
    if (Expected == "")
      return true;
#endif
    for (auto &B : CS.Blocks) {
      for (auto &R : B->Replacements) {
        std::string Str;
        llvm::raw_string_ostream SS(Str);
        R.print(SS);

        if (SS.str() == Expected)
          return true;
      }
    }
    return false;
  }

  bool hasCandidateExpr(std::string Expected) {
    for (const auto &Cand : CandExprs) {
      std::ostringstream SS;

      std::unique_ptr<ExprPPrinter> PP(ExprPPrinter::create(SS));
      PP->setForceNoLineBreaks(true);
      PP->scan(Cand->E);
      PP->print(Cand->E);

      if (SS.str() == Expected)
        return true;
    }
    return false;
  }
};

TEST_F(ExtractorTest, TriviallySat) {
  ASSERT_TRUE(extractFromIR(R"m(
define void @f(i32 %p) {
  %ule = icmp ule i32 %p, 0
  %sle = icmp sle i32 %p, 0
  %eq = icmp eq i32 %p, 0
  %ne = icmp ne i32 %p, 0
  ret void
}
)m"));

  EXPECT_EQ(0u, CandExprs.size());
}

TEST_F(ExtractorTest, Simple) {
  ASSERT_TRUE(extractFromIR(R"m(
define void @f(i32 %p, i32 %q) {
  %ult = icmp ult i32 %p, %q

  %add = add i32 %p, %q
  %ult1 = icmp ult i32 %p, %add

  ret void
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
define void @f(i32 %p, i32 %q) {
  %add = add nsw i32 %p, %q
  %ult = icmp ult i32 %p, %add
  ret void
}
)m"));

  EXPECT_TRUE(hasCandidateExpr(
      "(Or (And (Eq N0:(Extract 31 N1:(Read w32 0 p)) (Extract 31 N2:(Read w32 "
      "0 q))) (Eq false (Eq N0 (Extract 31 N3:(Add w32 N1 N2))))) (Ult N1 "
      "N3))"));
}

TEST_F(ExtractorTest, PhiCond) {
  ASSERT_TRUE(extractFromIR(R"m(
define void @f(i32 %p, i32 %q) {
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
  ret void
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

  EXPECT_EQ(0u, CandExprs.size());
}

TEST_F(ExtractorTest, PathCondition) {
  // Expected equivalence classes are {p, q, r} and {s}.
  ASSERT_TRUE(extractFromIR(R"m(
define void @f(i1 %p, i1 %q, i1 %r, i1 %s) {
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
  ret void

u:
  unreachable
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i1 = var ; p
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

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i1 = var ; s
pc %0 1:i1
%1:i1 = eq 1:i1, %0
cand %1 1:i1
)c"));
}

TEST_F(ExtractorTest, AddNuw) {
  ASSERT_TRUE(extractFromIR(R"m(
define i32 @foo(i32 %x) #0 {
entry:
  %add = add nuw i32 %x, 1
  %cmp = icmp ugt i32 %add, %x
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i32 = var ; x
%1:i32 = addnuw %0, 1:i32
%2:i1 = ult %0, %1
cand %2 1:i1
)c"));
}

TEST_F(ExtractorTest, Udiv) {
  ASSERT_TRUE(extractFromIR(R"m(
define i32 @mul(i32 %a, i32 %b) #0 {
entry:
  %div = udiv i32 %a, %b
  %cmp = icmp ult i32 %div, 0
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i32 = var ; a
%1:i32 = var ; b
%2:i32 = udiv %0, %1
%3:i1 = ult %2, 0:i32
cand %3 0:i1
)c"));
}

TEST_F(ExtractorTest, UdivExact) {
  ASSERT_TRUE(extractFromIR(R"m(
define i32 @fn1(i32 %a) #0 {
entry:
  %conv = zext i32 %a to i64
  %div = udiv exact i64 21474836490, %conv
  %tobool = icmp ne i64 %div, 0
  %land.ext = zext i1 %tobool to i32
  ret i32 %land.ext
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i32 = var ; a
%1:i64 = zext %0
%2:i64 = udivexact 21474836490:i64, %1
%3:i1 = ne 0:i64, %2
cand %3 1:i1
)c"));
}

TEST_F(ExtractorTest, Sdiv) {
  ASSERT_TRUE(extractFromIR(R"m(
define void @fn1(i32 %a, i32 %b, i32 %c, i32 %d) #0 {
entry:
  %div = sdiv i32 1, %a
  %cmp = icmp slt i32 %b, 0
  br i1 %cmp, label %land.rhs, label %land.end

land.rhs:                                         ; preds = %entry
  %cmp1 = icmp slt i32 %div, %b
  br label %land.end

land.end:                                         ; preds = %land.rhs, %entry
  %0 = phi i1 [ false, %entry ], [ %cmp1, %land.rhs ]
  %land.ext = zext i1 %0 to i32
  ret void
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i32 = var ; b
%1:i1 = slt %0, 0:i32
pc %1 1:i1
%2:i32 = var ; a
%3:i32 = sdiv 1:i32, %2
%4:i1 = slt %3, %0
cand %4 0:i1
)c"));
}

TEST_F(ExtractorTest, Sdiv2) {
  ASSERT_TRUE(extractFromIR(R"m(
define void @fn1(i32 %a, i32 %b, i16 signext %c) #0 {
entry:
  %tobool = icmp ne i32 %a, 0
  %lor.ext = zext i1 %tobool to i32
  %div = sdiv i32 %lor.ext, -2
  %conv = trunc i32 %div to i16
  %tobool1 = icmp ne i16 %conv, 0
  br i1 %tobool1, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  br label %if.end

if.end:                                           ; preds = %if.then, %entry
  ret void
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i32 = var ; a
%1:i1 = ne 0:i32, %0
%2:i32 = zext %1
%3:i32 = sdiv %2, 4294967294:i32
%4:i16 = trunc %3
%5:i1 = ne 0:i16, %4
cand %5 0:i1
)c"));
}

TEST_F(ExtractorTest, Sub) {
  ASSERT_TRUE(extractFromIR(R"m(
define i32 @foo(i32 %a, i32 %b) #0 {
entry:
  %sub = sub nuw i32 0, %a
  %sub1 = sub nuw i32 %sub, %b
  %cmp = icmp sle i32 %sub1, 0
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i32 = var ; a
%1:i32 = subnuw 0:i32, %0
%2:i32 = var ; b
%3:i32 = subnuw %1, %2
%4:i1 = sle %3, 0:i32
cand %4 1:i1
)c"));
}

TEST_F(ExtractorTest, Mul) {
  ASSERT_TRUE(extractFromIR(R"m(
define i32 @foo(i32 %a, i32 %b) #0 {
entry:
  %mul = mul nsw i32 %a, -2
  %cmp = icmp eq i32 %mul, 1
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i32 = var ; a
%1:i32 = mulnsw %0, 4294967294:i32
%2:i1 = eq 1:i32, %1
cand %2 0:i1
)c"));
}

TEST_F(ExtractorTest, Mul16Bit) {
  ASSERT_TRUE(extractFromIR(R"m(
define i16 @foo(i16 %x) #0 {
entry:
  %cmp = icmp slt i16 %x, 0
  br i1 %cmp, label %land.rhs, label %land.end

land.rhs:                                         ; preds = %entry
  %mul = mul nsw i16 %x, %x
  %cmp1 = icmp sge i16 %mul, %x
  br label %land.end

land.end:                                         ; preds = %land.rhs, %entry
  %0 = phi i1 [ false, %entry ], [ %cmp1, %land.rhs ]
  %land.ext = zext i1 %0 to i16
  ret i16 %land.ext
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i16 = var ; x
%1:i1 = slt %0, 0:i16
pc %1 1:i1
%2:i16 = mulnsw %0, %0
%3:i1 = sle %0, %2
cand %3 1:i1
)c"));
}
#if 0
TEST_F(ExtractorTest, Failed1) {
  ASSERT_TRUE(extractFromIR(R"m(
define i32 @checked_add_2(i32 %a) #0 {
entry:
  %add = add i32 %a, 1
  %cmp = icmp sgt i32 %add, 0
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c()c"));
}

TEST_F(ExtractorTest, Failed2) {
  ASSERT_TRUE(extractFromIR(R"m(
define i32 @foo(i32 %x) #0 {
entry:
  %mul = mul nuw i32 %x, %x
  %cmp = icmp sge i32 %mul, %x
  %conv = zext i1 %cmp to i32
  ret i32 %conv
}
)m"));
#endif
  EXPECT_TRUE(hasCandidate(R"c()c"));
}

TEST_F(ExtractorTest, Shift) {
  ASSERT_TRUE(extractFromIR(R"m(
define void @fn1(i32 %a) #0 {
entry:
  %shr = ashr exact i32 2, %a
  %cmp = icmp sgt i32 %shr, 2
  %conv = zext i1 %cmp to i32
  ret void
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i32 = var ; a
%1:i32 = ashrexact 2:i32, %0
%2:i1 = slt 2:i32, %1
cand %2 0:i1
)c"));
}

TEST_F(ExtractorTest, udiv_cornercase) {
  ASSERT_TRUE(extractFromIR(R"m(
define i32 @foo(i32 %x) #0 {
entry:
  %div = udiv i32 -2147483648, -1
  %tobool = icmp ne i32 %div, 0
  br i1 %tobool, label %cond.true, label %cond.false

cond.true:                                        ; preds = %entry
  br label %cond.end

cond.false:                                       ; preds = %entry
  br label %cond.end

cond.end:                                         ; preds = %cond.false, %cond.true
  %cond = phi i32 [ %div, %cond.true ], [ 0, %cond.false ]
  ret i32 %cond
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i32 = udiv 2147483648:i32, 4294967295:i32
%1:i1 = ne 0:i32, %0
cand %1 0:i1
)c"));
}

TEST_F(ExtractorTest, sdiv_UBcase) {
  ASSERT_TRUE(extractFromIR(R"m(
define i32 @foo(i32 %x) #0 {
entry:
  %div = sdiv i32 -2147483648, -1
  %tobool = icmp ne i32 %div, 0
  br i1 %tobool, label %cond.true, label %cond.false

cond.true:                                        ; preds = %entry
  br label %cond.end

cond.false:                                       ; preds = %entry
  br label %cond.end

cond.end:                                         ; preds = %cond.false, %cond.true
  %cond = phi i32 [ %div, %cond.true ], [ 0, %cond.false ]
  ret i32 %cond
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i32 = sdiv 2147483648:i32, 4294967295:i32
%1:i1 = ne 0:i32, %0
cand %1 0:i1
)c"));

  EXPECT_TRUE(hasCandidate(R"c(; Priority: 1
%0:i32 = sdiv 2147483648:i32, 4294967295:i32
%1:i1 = ne 0:i32, %0
cand %1 1:i1
)c"));
}
#if 0
TEST_F(ExtractorTest, sdiv_UBcase2) {
  ASSERT_TRUE(extractFromIR(R"m(
define i32 @foo(i32 %x) #0 {
entry:
  %div = sdiv i32 %x, -1
  %tobool = icmp ne i32 %div, 0
  br i1 %tobool, label %cond.true, label %cond.false

cond.true:                                        ; preds = %entry
  br label %cond.end

cond.false:                                       ; preds = %entry
  br label %cond.end

cond.end:                                         ; preds = %cond.false, %cond.true
  %cond = phi i32 [ %div, %cond.true ], [ 1, %cond.false ]
  ret i32 %cond
}
)m"));

  EXPECT_TRUE(hasCandidate(R"c()c"));
}
#endif
