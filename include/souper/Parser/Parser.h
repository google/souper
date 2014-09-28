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

  void print(llvm::raw_ostream &OS, bool Partial) const {
    PrintReplacement(OS, PCs, Mapping, Partial);
  }
  std::string getString(bool Partial) const {
    return GetReplacementString(PCs, Mapping, Partial);
  }
  void printResult(llvm::raw_ostream &OS) const {
    PrintReplacementResult(OS, PCs, Mapping);
  }
  std::string getResultString() const {
    return GetReplacementResultString(PCs, Mapping);
  }
};

void TestLexer(llvm::StringRef Str);

ParsedReplacement ParseReplacement(InstContext &IC, llvm::StringRef Filename,
                                   llvm::StringRef Str, std::string &ErrStr,
                                   bool Partial);

std::vector<ParsedReplacement> ParseReplacements(InstContext &IC,
                                                 llvm::StringRef Filename,
                                                 llvm::StringRef Str,
                                                 std::string &ErrStr,
                                                 bool Partial);

}

#endif  // SOUPER_PARSER_PARSER_H
