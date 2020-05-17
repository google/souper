#ifndef SOUPER_PRECONDITIONS_H
#define SOUPER_PRECONDITIONS_H

#include "souper/Inst/Inst.h"
#include "llvm/Support/KnownBits.h"
extern unsigned DebugLevel;
namespace souper {
class SMTLIBSolver;
class Solver;
std::vector<std::map<Inst *, llvm::KnownBits>>
  inferAbstractKBPreconditions(SynthesisContext &SC, Inst *RHS,
                               SMTLIBSolver *SMTSolver, Solver *S, bool &FoundWeakest);
}

#endif // SOUPER_PRECONDITIONS_H
