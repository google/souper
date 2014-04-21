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

std::unique_ptr<Module> ParseIR(const char *IR) {
  LLVMContext &C = getGlobalContext();
  SMDiagnostic Err;
  auto M = ParseAssemblyString(IR, 0, Err, C);
  if (!M) {
    Err.print("ExtractorTest", errs());
  }
  return std::unique_ptr<Module>(M);
}

std::vector<std::unique_ptr<CandidateExpr>> ExtractExprs(Module *M) {
  InstContext IC;
  ExprBuilderContext IBC;
  std::vector<std::unique_ptr<CandidateExpr>> Result;

  ExprBuilderOptions Opts;
  Opts.NamedArrays = true;

  for (auto &F : *M) {
    auto CS = ExtractCandidates(&F, IC, IBC, Opts);
    for (auto &B : CS.Blocks) {
      for (auto &R : B->Replacements) {
        std::unique_ptr<CandidateExpr> CE(
            new CandidateExpr(GetCandidateExprForReplacement(R)));
        if (!IsTriviallyInvalid(CE->E)) {
          Result.emplace_back(std::move(CE));
        }
      }
    }
  }

  return Result;
}

bool HasExprCandidate(const std::vector<std::unique_ptr<CandidateExpr>> &Cands,
                      std::string Expected) {
  for (const auto &Cand : Cands) {
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

TEST(ExtractorTest, TriviallySat) {
  auto M = ParseIR(R"m(
define void @f(i32 %p) {
  %ule = icmp ule i32 %p, 0
  %sle = icmp sle i32 %p, 0
  %eq = icmp eq i32 %p, 0
  %ne = icmp ne i32 %p, 0
  ret void
}
)m");
  ASSERT_TRUE(M.get());

  auto EC = ExtractExprs(M.get());

  EXPECT_EQ(0u, EC.size());
}

TEST(ExtractorTest, Simple) {
  auto M = ParseIR(R"m(
define void @f(i32 %p, i32 %q) {
  %ult = icmp ult i32 %p, %q

  %add = add i32 %p, %q
  %ult1 = icmp ult i32 %p, %add

  ret void
}
)m");
  ASSERT_TRUE(M.get());

  auto EC = ExtractExprs(M.get());

  EXPECT_TRUE(
      HasExprCandidate(EC, "(Ult (Read w32 0 p) (Read w32 0 q))"));
  EXPECT_TRUE(HasExprCandidate(
      EC, "(Eq false (Ult (Read w32 0 p) (Read w32 0 q)))"));

  EXPECT_TRUE(HasExprCandidate(
      EC, "(Ult N0:(Read w32 0 p) (Add w32 N0 (Read w32 0 q)))"));
}

TEST(ExtractorTest, Nsw) {
  auto M = ParseIR(R"m(
define void @f(i32 %p, i32 %q) {
  %add = add nsw i32 %p, %q
  %ult = icmp ult i32 %p, %add
  ret void
}
)m");
  ASSERT_TRUE(M.get());

  auto EC = ExtractExprs(M.get());

  EXPECT_TRUE(HasExprCandidate(EC,
                               "(Or (And (Eq N0:(Extract 31 N1:(Read w32 0 p)) "
                               "(Extract 31 N2:(Read w32 0 q))) (Eq false (Eq "
                               "N0 (Extract 31 N3:(Add w32 N1 N2))))) (Ult N1 "
                               "N3))"));
}

TEST(ExtractorTest, PhiCond) {
  auto M = ParseIR(R"m(
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
)m");
  ASSERT_TRUE(M.get());

  auto EC = ExtractExprs(M.get());

  EXPECT_TRUE(HasExprCandidate(EC,
                               "(Ult N0:(Read w32 0 p) (Select w32 (Read 0 "
                               "blockpred) (Add w32 N0 N1:(Read w32 0 q)) (Mul "
                               "w32 N0 N1)))"));
}

TEST(ExtractorTest, PhiLoop) {
  auto M = ParseIR(R"m(
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
)m");
  ASSERT_TRUE(M.get());

  auto EC = ExtractExprs(M.get());

  EXPECT_EQ(0u, EC.size());
}
