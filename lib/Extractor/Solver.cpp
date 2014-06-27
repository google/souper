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

#define DEBUG_TYPE "souper"

#include "llvm/ADT/Statistic.h"
#include "souper/Extractor/Solver.h"

#include <unordered_map>

STATISTIC(Hits, "Number of internal cache hits");
STATISTIC(Misses, "Number of internal cache misses");

using namespace souper;

namespace {

class BaseSolver : public Solver {
  std::unique_ptr<SMTLIBSolver> SMTSolver;
  unsigned Timeout;

public:
  BaseSolver(std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout)
      : SMTSolver(std::move(SMTSolver)), Timeout(Timeout) {}

  llvm::error_code isValid(const std::vector<InstMapping> &PCs,
                           InstMapping Mapping, bool &IsValid) {
    bool IsSat;
    llvm::error_code EC = SMTSolver->isSatisfiable(BuildQuery (PCs, Mapping),
                                                   IsSat, Timeout);
    IsValid = !IsSat;
    return EC;
  }

  std::string getName() {
    return SMTSolver->getName();
  }
};

class CachingSolver : public Solver {
  std::unique_ptr<Solver> UnderlyingSolver;
  std::unordered_map<std::string,std::pair<llvm::error_code,bool>> Cache;

public:
  CachingSolver(std::unique_ptr<Solver> UnderlyingSolver)
      : UnderlyingSolver(std::move(UnderlyingSolver)) {}

  llvm::error_code isValid(const std::vector<InstMapping> &PCs,
                           InstMapping Mapping, bool &IsValid) {
    std::string buf;
    llvm::raw_string_ostream OS(buf);
    souper::PrintReplacement(OS, PCs, Mapping);

    const auto &ent = Cache.find(OS.str());
    if (ent == Cache.end()) {
      ++Misses;
      llvm::error_code EC = UnderlyingSolver->isValid(PCs, Mapping, IsValid);
      Cache.emplace (OS.str(), std::make_pair (EC, IsValid));
      return EC;
    } else {
      ++Hits;
      IsValid = ent->second.second;
      return ent->second.first;
    }
  }

  std::string getName() {
    return UnderlyingSolver->getName() + " + internal cache";
  }

};

}

namespace souper {

Solver::~Solver() {}

std::unique_ptr<Solver> createBaseSolver(
    std::unique_ptr<SMTLIBSolver> SMTSolver, unsigned Timeout) {
  return std::unique_ptr<Solver>(new BaseSolver(std::move(SMTSolver), Timeout));
}

std::unique_ptr<Solver> createCachingSolver(
    std::unique_ptr<Solver> UnderlyingSolver) {
  return std::unique_ptr<Solver>(
      new CachingSolver(std::move(UnderlyingSolver)));
}

}
