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

#ifndef SOUPER_UTIL_DFAUTILS_H
#define SOUPER_UTIL_DFAUTILS_H

#include "llvm/Support/CommandLine.h"

using namespace llvm;

static cl::opt<bool> InferNeg("infer-neg",
    cl::desc("Compute Negative for the candidate (default=false)"),
    cl::init(false));

static cl::opt<bool> InferKnownBits("infer-known-bits",
    cl::desc("Compute known bits for the candidate (default=false)"),
    cl::init(false));

static cl::opt<bool> InferNonNeg("infer-non-neg",
   cl::desc("Compute non-negative for the candidate (default=false)"),
    cl::init(false));

static cl::opt<bool> InferPowerTwo("infer-power-two",
    cl::desc("Compute power of two for the candidate (default=false)"),
    cl::init(false));

static cl::opt<bool> InferNonZero("infer-non-zero",
    cl::desc("Compute non zero for the candidate (default=false)"),
    cl::init(false));

static cl::opt<bool> InferSignBits("infer-sign-bits",
    cl::desc("Compute sign bits for the candidate (default=false)"),
    cl::init(false));

static cl::opt<bool> InferRange("infer-range",
    cl::desc("Compute range for the candidate (default=false)"),
    cl::init(false));

static cl::opt<bool> InferDemandedBits("infer-demanded-bits",
    cl::desc("Compute demanded bits for the candidate (default=false)"),
    cl::init(false));

static bool isInferDFA() {
  return InferNeg || InferNonNeg || InferKnownBits || InferPowerTwo ||
         InferNonZero || InferSignBits || InferRange  || InferDemandedBits;
}

static std::string convertToStr(bool Fact) {
    if (Fact)
      return "true";
    else
      return "false";
}

#endif
