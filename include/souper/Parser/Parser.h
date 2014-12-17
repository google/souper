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

#ifndef SOUPER_PARSER_PARSER_H
#define SOUPER_PARSER_PARSER_H

#include "llvm/ADT/StringRef.h"
#include "souper/Extractor/Candidates.h"

namespace souper {

struct ParsedReplacement {
  /// The replacement mapping.
  InstMapping Mapping;

  /// The path conditions relevant to this replacement.
  std::vector<InstMapping> PCs;

  /// The blockpc condtions relevant to this replacement.
  BlockPCs BPCs;

  void print(llvm::raw_ostream &OS, bool printNames = false) const {
    PrintReplacement(OS, BPCs, PCs, Mapping, printNames);
  }
  std::string getString(bool printNames = false) const {
    return GetReplacementString(BPCs, PCs, Mapping, printNames);
  }
  void printLHS(llvm::raw_ostream &OS, ReplacementContext &Context,
                bool printNames = false) const {
    PrintReplacementLHS(OS, BPCs, PCs, Mapping.LHS, Context, printNames);
  }
  std::string getLHSString(ReplacementContext &Context,
                           bool printNames = false) const {
    return GetReplacementLHSString(BPCs, PCs, Mapping.LHS, Context, printNames);
  }
  void printRHS(llvm::raw_ostream &OS, ReplacementContext &Context,
                bool printNames = false) const {
    PrintReplacementRHS(OS, Mapping.RHS, Context, printNames);
  }
  std::string getRHSString(ReplacementContext &Context,
                           bool printNames = false) const {
    return GetReplacementRHSString(Mapping.RHS, Context, printNames);
  }
};

void TestLexer(llvm::StringRef Str);

ParsedReplacement ParseReplacement(InstContext &IC, llvm::StringRef Filename,
                                   llvm::StringRef Str, std::string &ErrStr);
ParsedReplacement ParseReplacementLHS(InstContext &IC, llvm::StringRef Filename,
                                      llvm::StringRef Str,
                                      ReplacementContext &Pr,
                                      std::string &ErrStr);
ParsedReplacement ParseReplacementRHS(InstContext &IC, llvm::StringRef Filename,
                                      llvm::StringRef Str,
                                      ReplacementContext &Pr,
                                      std::string &ErrStr);

std::vector<ParsedReplacement> ParseReplacements(InstContext &IC,
    llvm::StringRef Filename, llvm::StringRef Str, std::string &ErrStr);
std::vector<ParsedReplacement> ParseReplacementLHSs(InstContext &IC,
    llvm::StringRef Filename, llvm::StringRef Str,
    std::vector<ReplacementContext> &Contexts, std::string &ErrStr);
std::vector<ParsedReplacement> ParseReplacementRHSs(InstContext &IC,
    llvm::StringRef Filename, llvm::StringRef Str,
    std::vector<ReplacementContext> &Contexts, std::string &ErrStr);

}

#endif  // SOUPER_PARSER_PARSER_H
