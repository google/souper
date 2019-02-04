// Copyright 2019 The Souper Authors. All rights reserved.
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

// This file contains auxiliary tool that prints the number of times each
// instruction occurs in LHS / RHS. The instructions are ordered according to
// the enum in Inst.h
// Output: first line is number of occurrences of each
// instruction on the LHS, second is same for RHS

#include "souper/Parser/Parser.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <unistd.h>

using namespace souper;
using namespace llvm;

void countHelper(Inst *I, std::set<Inst *> &Visited,
                 std::map<int, int> &Result) {
  if (!Visited.insert(I).second)
    return;

  ++Result[I->K];

  for (auto Op : I->Ops)
    countHelper(Op, Visited, Result);
}

void instCount(Inst *I, std::map<int, int> &Result) {
  std::set<Inst *> Visited;
  return countHelper(I, Visited, Result);
}

int main(int argc, char **argv) {
  if (argc != 2)
    std::cerr << "Please specify input file!" << '\n';

  auto MB = MemoryBuffer::getFileOrSTDIN(argv[1]);

  if (MB) {
    InstContext IC;
    std::string ErrStr;
    std::vector<ParsedReplacement> Reps;
    std::vector<ReplacementContext> Contexts;
    Reps = ParseReplacements(IC, MB.get()->getBufferIdentifier(),
                             MB.get()->getBuffer(), ErrStr);
    if (!ErrStr.empty()) {
      llvm::errs() << ErrStr << '\n';
      return 1;
    }

    std::map<int, int> LHSResult;
    std::map<int, int> RHSResult;

    for (const auto &R : Reps) {
      instCount(R.Mapping.LHS, LHSResult);
      instCount(R.Mapping.RHS, RHSResult);

      for (const auto &PC : R.PCs) {
        instCount(PC.LHS, LHSResult);
        instCount(PC.RHS, RHSResult);
      }

      for (const auto &BPC : R.BPCs) {
        instCount(BPC.PC.LHS, LHSResult);
        instCount(BPC.PC.RHS, RHSResult);
      }
    }

    for (Inst::Kind i = Inst::Const; i <= Inst::None;
         i = static_cast<Inst::Kind>(static_cast<int>(i) + 1))
      std::cout << LHSResult[i] << (i != Inst::None ? "," : "");
    std::cout << std::endl;

    for (Inst::Kind i = Inst::Const; i <= Inst::None;
         i = static_cast<Inst::Kind>(static_cast<int>(i) + 1))
      std::cout << RHSResult[i] << (i != Inst::None ? "," : "");
    std::cout << std::endl;
  }
  else {
    std::cerr << MB.getError().message() << '\n';
  }
  return 0;
}
