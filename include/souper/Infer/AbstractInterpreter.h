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
