#ifndef SOUPER_PRECONDITIONS_H
#define SOUPER_PRECONDITIONS_H

#include "souper/Inst/Inst.h"
#include "llvm/Support/KnownBits.h"
#include "llvm/IR/ConstantRange.h"
extern unsigned DebugLevel;
namespace souper {
class SMTLIBSolver;
class Solver;

std::pair<std::vector<std::map<Inst *, llvm::KnownBits>>,
std::vector<std::map<Inst *, llvm::ConstantRange>>>
inferAbstractPreconditions(SynthesisContext &SC, Inst *RHS,
                               Solver *S, bool &FoundWeakest);

std::vector<std::map<Inst *, llvm::KnownBits>>
  inferAbstractKBPreconditions(SynthesisContext &SC, Inst *RHS,
                               Solver *S, bool &FoundWeakest);
std::vector<std::map<Inst *, llvm::ConstantRange>>
  inferAbstractCRPreconditions(SynthesisContext &SC, Inst *RHS,
                               Solver *S, bool &FoundWeakest);
}

#endif // SOUPER_PRECONDITIONS_H
