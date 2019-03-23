#ifndef SOUPER_PRUNING_H
#define SOUPER_PRUNING_H

#include "llvm/ADT/APInt.h"

#include "souper/Extractor/Solver.h"
#include "souper/Infer/Interpreter.h"
#include "souper/Inst/Inst.h"

#include <unordered_map>

namespace souper {

typedef std::function<bool(Inst *, std::vector<Inst *> &)> PruneFunc;

class PruningManager {
public:
  PruningManager(Inst *LHS_, std::vector< souper::Inst* >& Inputs_,
                 unsigned int StatsLevel_, InstContext& IC_,
                 SMTLIBSolver *SMTSolver_);
  PruneFunc getPruneFunc() {return DataflowPrune;}
  void printStats(llvm::raw_ostream &out) {
    out << "Dataflow Pruned " << NumPruned << "/" << TotalGuesses << "\n";
  }

  bool isInfeasible(Inst *RHS, unsigned StatsLevel);
  bool isInfeasibleWithSolver(Inst *RHS, unsigned StatsLevel);
  void init();
  // double init antipattern, required because init should
  // not be called when pruning is disabled
private:
  Inst *LHS;
  std::vector<EvalValue> LHSValues;
  std::vector<llvm::KnownBits> LHSKnownBits;
  std::vector<llvm::ConstantRange> LHSConstantRange;
  bool LHSHasPhi = false;

  InstContext &IC;
  SMTLIBSolver *S;
  PruneFunc DataflowPrune;
  unsigned NumPruned;
  unsigned TotalGuesses;
  int StatsLevel;
  SMTLIBSolver *SMTSolver;
  std::vector<ValueCache> InputVals;
  std::vector<Inst *> &InputVars;
  std::vector<ValueCache> generateInputSets(std::vector<Inst *> &Inputs);
};

}

#endif
