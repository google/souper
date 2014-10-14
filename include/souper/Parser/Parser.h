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

  void print(llvm::raw_ostream &OS) const {
    PrintReplacement(OS, PCs, Mapping);
  }
  std::string getString() const {
    return GetReplacementString(PCs, Mapping);
  }
  void printLHS(llvm::raw_ostream &OS) const {
    PrintReplacementLHS(OS, PCs, Mapping.LHS);
  }
  std::string getLHSString() const {
    return GetReplacementLHSString(PCs, Mapping.LHS);
  }
  void printRHS(llvm::raw_ostream &OS) const {
    assert(Mapping.RHS->K == Inst::Const);
    PrintReplacementRHS(OS, Mapping.RHS->Val);
  }
  std::string getRHSString() const {
    assert(Mapping.RHS->K == Inst::Const);
    return GetReplacementRHSString(Mapping.RHS->Val);
  }
};

void TestLexer(llvm::StringRef Str);

ParsedReplacement ParseReplacement(InstContext &IC, llvm::StringRef Filename,
                                   llvm::StringRef Str, std::string &ErrStr);
ParsedReplacement ParseReplacementLHS(InstContext &IC, llvm::StringRef Filename,
                                      llvm::StringRef Str, std::string &ErrStr);
ParsedReplacement ParseReplacementRHS(InstContext &IC, llvm::StringRef Filename,
                                      llvm::StringRef Str, std::string &ErrStr);

std::vector<ParsedReplacement> ParseReplacements(InstContext &IC,
                                                 llvm::StringRef Filename,
                                                 llvm::StringRef Str,
                                                 std::string &ErrStr);
std::vector<ParsedReplacement> ParseReplacementLHSs(InstContext &IC,
                                                    llvm::StringRef Filename,
                                                    llvm::StringRef Str,
                                                    std::string &ErrStr);
std::vector<ParsedReplacement> ParseReplacementRHSs(InstContext &IC,
                                                    llvm::StringRef Filename,
                                                    llvm::StringRef Str,
                                                    std::string &ErrStr);

}

#endif  // SOUPER_PARSER_PARSER_H
