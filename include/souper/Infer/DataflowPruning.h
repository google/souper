#include "llvm/ADT/APInt.h"

#include "souper/Infer/Interpreter.h"
#include "souper/Inst/Inst.h"

#include <unordered_map>

namespace souper {

class ValueAnalysis {
public:
  ValueAnalysis(Inst *LHS_, std::vector<ValueCache> Inputs_)
    : LHS(LHS_),Inputs(Inputs_) {
    for (auto &&Input : Inputs) {
      LHSValues.push_back(evaluateInst(LHS, Input));
    }
  }
  bool isInfeasible(Inst *RHS);
private:
  Inst *LHS;
  std::vector<EvalValue> LHSValues;
  std::vector<ValueCache> Inputs;
};



typedef std::function<bool(Inst *, std::vector<Inst *> &)> PruneFunc;

class DataflowPruningManager {
public:
  DataflowPruningManager(Inst *LHS, std::vector<Inst *> &Inputs,
                         unsigned StatsLevel);
  PruneFunc getPruneFunc() {return DataflowPrune;}
  void printStats(llvm::raw_ostream &out) {
    out << "Dataflow Pruned " << NumPruned << "/" << TotalGuesses << "\n";
  }
private:
  ValueAnalysis VA;
  PruneFunc DataflowPrune;
  unsigned NumPruned;
  unsigned TotalGuesses;

  std::vector<ValueCache> generateInputSets(std::vector<Inst *> &Inputs);
};

}
