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

#ifndef SOUPER_ABSTRACT_INTERPRTER_H
#define SOUPER_ABSTRACT_INTERPRTER_H

#include "llvm/Support/KnownBits.h"

namespace souper {
  namespace BinaryTransferFunctionsKB {
    llvm::KnownBits add(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits addnsw(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits sub(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits subnsw(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits mul(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits udiv(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits urem(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits and_(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits or_(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits xor_(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits shl(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits lshr(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits ashr(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits eq(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits ne(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits ult(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits slt(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits ule(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
    llvm::KnownBits sle(const llvm::KnownBits &lhs, const llvm::KnownBits &rhs);
  }
}

#endif
