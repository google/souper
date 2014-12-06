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

#ifndef SOUPER_UTIL_UNIQUENAMESET_H
#define SOUPER_UTIL_UNIQUENAMESET_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"

namespace souper {

class UniqueNameSet {
  llvm::StringSet<> Names;

public:
  std::string makeName(llvm::StringRef OrigName) {
    if (!OrigName.empty() && Names.insert(OrigName).second)
      return OrigName;

    unsigned i = 0;
    while (true) {
      std::string S = OrigName;
      llvm::raw_string_ostream SS(S);
      SS << i;
      if (Names.insert(SS.str()).second)
        return S;
      ++i;
    }
  }

};

}

#endif
