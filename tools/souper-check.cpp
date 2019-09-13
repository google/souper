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

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/KnownBits.h"

#include "souper/Infer/ConstantSynthesis.h"
#include "souper/Inst/InstGraph.h"
#include "souper/Parser/Parser.h"
#include "souper/Tool/GetSolverFromArgs.h"

using namespace llvm;
using namespace souper;

static cl::opt<std::string>
InputFilename(cl::Positional, cl::desc("<input souper optimization>"),
              cl::init("-"));

static cl::opt<bool> PrintCounterExample("print-counterexample",
    cl::desc("Print counterexample (default=true)"),
    cl::init(true));

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

static cl::opt<bool> PrintRepl("print-replacement",
    cl::desc("Print the replacement, if valid (default=false)"),
    cl::init(false));

static cl::opt<bool> PrintReplSplit("print-replacement-split", cl::Hidden,
    cl::desc("Print the replacement as an infer/result pair (default=false)"),
    cl::init(false));

static cl::opt<bool> InferRHS("infer-rhs",
    cl::desc("Try to infer a RHS for a Souper LHS (default=false)"),
    cl::init(false));

static cl::opt<bool> InferConst("infer-const",
    cl::desc("Try to infer constants for a Souper replacement (default=false)"),
    cl::init(false));

static cl::opt<bool> ReInferRHS("reinfer-rhs",
    cl::desc("Try to infer a new RHS and compare its cost with the existing RHS (default=false)"),
    cl::init(false));

static cl::opt<bool> ParseOnly("parse-only",
    cl::desc("Only parse the replacement, don't call isValid() (default=false)"),
    cl::init(false));

static cl::opt<bool> ParseLHSOnly("parse-lhs-only",
    cl::desc("Only parse the LHS, don't call infer() (default=false)"),
    cl::init(false));

static cl::opt<bool> EmitLHSDot("emit-lhs-dot",
    cl::desc("Emit DOT format DAG for LHS of given souper IR (default=false)"),
    cl::init(false));

std::string convertToStr(bool Fact) {
    if (Fact)
      return "true";
    else
      return "false";
}

int SolveInst(const MemoryBufferRef &MB, Solver *S) {
  InstContext IC;
  std::string ErrStr;

  std::vector<ParsedReplacement> Reps;
  std::vector<ReplacementContext> Contexts;
  if (InferRHS || ParseLHSOnly || InferNeg || InferNonNeg ||
      InferKnownBits || InferPowerTwo || InferNonZero ||
      InferSignBits || InferRange) {
    Reps = ParseReplacementLHSs(IC, MB.getBufferIdentifier(), MB.getBuffer(),
                                Contexts, ErrStr);
  } else {
    Reps = ParseReplacements(IC, MB.getBufferIdentifier(), MB.getBuffer(), ErrStr);
  }
  if (!ErrStr.empty()) {
    llvm::errs() << ErrStr << '\n';
    return 1;
  }

  if (EmitLHSDot) {
    llvm::outs() << "; emitting DOT for parsed LHS souper IR ...\n";
    for (auto &Rep : Reps) {
      llvm::WriteGraph(llvm::outs(), Rep.Mapping.LHS);
    }
  }

  if (ParseOnly || ParseLHSOnly) {
    llvm::outs() << "; parsing successful\n";
    return 0;
  }

  unsigned Index = 0;
  int Ret = 0;
  int Success = 0, Fail = 0, Error = 0;
  for (auto Rep : Reps) {
    if (InferNeg) {
      bool Negative;
      if (std::error_code EC = S->negative(Rep.BPCs, Rep.PCs, Rep.Mapping.LHS,
                                           Negative, IC)) {
        llvm::errs() << "Error: " << EC.message() << '\n';
        Ret = 1;
        ++Error;
      } else {
        llvm::outs() << "known negative from souper: "
                     << convertToStr(Negative) << "\n";
        ++Success;
      }
    } else if (InferNonNeg) {
      bool NonNegative;
      if (std::error_code EC = S->nonNegative(Rep.BPCs, Rep.PCs, Rep.Mapping.LHS,
                                              NonNegative, IC)) {
        llvm::errs() << "Error: " << EC.message() << '\n';
        Ret = 1;
        ++Error;
      } else {
        llvm::outs() << "known nonNegative from souper: "
                     << convertToStr(NonNegative) << "\n";
        ++Success;
      }
    } else if (InferKnownBits) {
      unsigned W = Rep.Mapping.LHS->Width;
      KnownBits Known(W);
      if (std::error_code EC = S->knownBits(Rep.BPCs, Rep.PCs, Rep.Mapping.LHS,
                                            Known, IC)) {
        llvm::errs() << "Error: " << EC.message() << '\n';
        Ret = 1;
        ++Error;
      } else {
        llvm::outs() << "knownBits from souper: "
                     << Inst::getKnownBitsString(Known.Zero, Known.One) << "\n";
        ++Success;
      }
    } else if (InferPowerTwo) {
      bool PowTwo;
      if (std::error_code EC = S->powerTwo(Rep.BPCs, Rep.PCs, Rep.Mapping.LHS,
                                           PowTwo, IC)) {
        llvm::errs() << "Error: " << EC.message() << '\n';
        Ret = 1;
        ++Error;
      } else {
        llvm::outs() << "known powerOfTwo from souper: "
                     << convertToStr(PowTwo) << "\n";
        ++Success;
      }
    } else if (InferNonZero) {
      bool NonZero;
      if (std::error_code EC = S->nonZero(Rep.BPCs, Rep.PCs, Rep.Mapping.LHS,
                                          NonZero, IC)) {
        llvm::errs() << "Error: " << EC.message() << '\n';
        Ret = 1;
        ++Error;
      } else {
        llvm::outs() << "known nonZero from souper: "
                     << convertToStr(NonZero) << "\n";
        ++Success;
      }
    } else if (InferSignBits) {
      unsigned SignBits;
      if (std::error_code EC = S->signBits(Rep.BPCs, Rep.PCs, Rep.Mapping.LHS,
                                           SignBits, IC)) {
        llvm::errs() << "Error: " << EC.message() << '\n';
        Ret = 1;
        ++Error;
      } else {
        llvm::outs() << "known signBits from souper: "
                     << std::to_string(SignBits) << "\n";
        ++Success;
      }
    } else if (InferRange) {
      unsigned W = Rep.Mapping.LHS->Width;
      llvm::ConstantRange Range = S->constantRange(Rep.BPCs, Rep.PCs, Rep.Mapping.LHS, IC);

      llvm::outs() << "known range from souper: " << "[" << Range.getLower()
                   << "," << Range.getUpper() << ")" << "\n";
      ++Success;
    } else if (InferRHS || ReInferRHS) {
      int OldCost;
      if (ReInferRHS) {
        OldCost = cost(Rep.Mapping.RHS);
        Rep.Mapping.RHS = 0;
      }
      if (std::error_code EC = S->infer(Rep.BPCs, Rep.PCs, Rep.Mapping.LHS,
                                        Rep.Mapping.RHS, IC)) {
        llvm::errs() << EC.message() << '\n';
        Ret = 1;
        ++Error;
      }
      if (Rep.Mapping.RHS) {
        ++Success;
        if (ReInferRHS) {
          int NewCost = cost(Rep.Mapping.RHS);
          int LHSCost = cost(Rep.Mapping.LHS);
          if (NewCost <= OldCost)
            llvm::outs() << "; RHS inferred successfully, no cost regression";
          else
            llvm::outs() << "; RHS inferred successfully, but cost regressed";
          llvm::outs() << " (Old= " << OldCost << ", New= " << NewCost <<
            ", LHS= " << LHSCost << ")\n";
        } else {
          llvm::outs() << "; RHS inferred successfully\n";
        }
        if (PrintRepl) {
          PrintReplacement(llvm::outs(), Rep.BPCs, Rep.PCs, Rep.Mapping);
        } else if (PrintReplSplit) {
          ReplacementContext Context;
          PrintReplacementLHS(llvm::outs(), Rep.BPCs, Rep.PCs,
                              Rep.Mapping.LHS, Context);
          PrintReplacementRHS(llvm::outs(), Rep.Mapping.RHS, Context);
        } else {
          ReplacementContext Context;
          PrintReplacementRHS(llvm::outs(), Rep.Mapping.RHS,
                              ReInferRHS ? Context : Contexts[Index]);
        }
      } else {
        ++Fail;
        llvm::outs() << "; Failed to infer RHS\n";
        if (PrintRepl || PrintReplSplit) {
          ReplacementContext Context;
          PrintReplacementLHS(llvm::outs(), Rep.BPCs, Rep.PCs,
                              Rep.Mapping.LHS, Context);
        }
      }
    } else if (InferConst) {
      ConstantSynthesis CS;
      std::map <Inst *, llvm::APInt> ResultConstMap;

      std::set<Inst *> ConstSet;
      souper::getConstants(Rep.Mapping.RHS, ConstSet);
      if (ConstSet.empty()) {
        llvm::outs() << "; No reservedconst found in RHS\n";
      } else {
        if (std::error_code EC = S->inferConst(Rep.BPCs, Rep.PCs,
                                               Rep.Mapping.LHS, Rep.Mapping.RHS,
                                               ConstSet, ResultConstMap, IC)) {
          llvm::errs() << EC.message() << '\n';
          Ret = 1;
          ++Error;
        }

        if (!ResultConstMap.empty()) {
          ReplacementContext Context;
          llvm::outs() << "; RHS inferred successfully\n";
          PrintReplacementRHS(llvm::outs(), Rep.Mapping.RHS, Context);
          ++Success;
        } else {
          ++Fail;
          llvm::outs() << "; Failed to infer RHS\n";
        }
      }
    } else {
      bool Valid;
      std::vector<std::pair<Inst *, APInt>> Models;
      if (std::error_code EC = S->isValid(IC, Rep.BPCs, Rep.PCs,
                                          Rep.Mapping, Valid, &Models)) {
        llvm::errs() << EC.message() << '\n';
        Ret = 1;
        ++Error;
      }

      if (Valid) {
        ++Success;
        llvm::outs() << "; LGTM\n";
        if (PrintRepl)
          PrintReplacement(llvm::outs(), Rep.BPCs, Rep.PCs, Rep.Mapping);
        if (PrintReplSplit) {
          ReplacementContext Context;
          PrintReplacementLHS(llvm::outs(), Rep.BPCs, Rep.PCs,
                              Rep.Mapping.LHS, Context);
          PrintReplacementRHS(llvm::outs(), Rep.Mapping.RHS, Context);
        }
      } else {
        ++Fail;
        llvm::outs() << "Invalid";
        if (PrintCounterExample && !Models.empty()) {
          llvm::outs() << ", e.g.\n\n";
          std::sort(Models.begin(), Models.end(),
                    [](const std::pair<Inst *, APInt> &A,
                       const std::pair<Inst *, APInt> &B) {
                      return A.first->Name < B.first->Name;
                    });
          for (const auto &M : Models) {
            llvm::outs() << '%' << M.first->Name << " = " << M.second << '\n';
          }
        } else {
          llvm::outs() << "\n";
        }
      }
    }
    ++Index;
    if (PrintRepl || PrintReplSplit)
      llvm::outs() << "\n";
  }
  if ((Success + Fail + Error) > 1)
    llvm::outs() << "successes = " << Success << ", failures = " << Fail <<
      ", errors = " << Error << "\n";
  return Ret;
}

int main(int argc, char **argv) {
  cl::ParseCommandLineOptions(argc, argv);
  KVStore *KV = 0;
  std::unique_ptr<Solver> S = 0;
  if (!ParseOnly && !ParseLHSOnly) {
    S = GetSolverFromArgs(KV);
    if (!S) {
      llvm::errs() << "Specify a solver\n";
      return 1;
    }
  }

  auto MB = MemoryBuffer::getFileOrSTDIN(InputFilename);
  if (!MB) {
    llvm::errs() << MB.getError().message() << '\n';
    return 1;
  }
  return SolveInst((*MB)->getMemBufferRef(), S.get());
}
