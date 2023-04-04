#ifndef SOUPER_GENERALIZE_REDUCER_H
#define SOUPER_GENERALIZE_REDUCER_H

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/KnownBits.h"
#include "souper/Parser/Parser.h"
#include "souper/Infer/EnumerativeSynthesis.h"
#include "souper/Infer/ConstantSynthesis.h"
#include "souper/Infer/Pruning.h"
#include "souper/Infer/SynthUtils.h"

namespace souper {

class Reducer {
public:
  Reducer(InstContext &IC_, Solver *S_) : IC(IC_), S(S_), varnum(0), numSolverCalls(0) {}

  ParsedReplacement ReduceGreedy(ParsedReplacement Input);

  ParsedReplacement ReducePairsGreedy(ParsedReplacement Input);

  ParsedReplacement ReduceTriplesGreedy(ParsedReplacement Input);

  // Eventually replace the functions in Preconditions{.h/.cpp} with this.
  // Does not produce exhaustive result. TODO Have an option to wrap in a cegis loop.
  bool inferKBPrecondition(ParsedReplacement &Input, std::vector<Inst *> Targets);

  ParsedReplacement ReduceGreedyKBIFY(ParsedReplacement Input);

  ParsedReplacement ReduceRedundantPhis(ParsedReplacement Input);

  // Assumes Input is valid
  ParsedReplacement ReducePCs(ParsedReplacement Input);

  ParsedReplacement WeakenKB(ParsedReplacement Input);

  ParsedReplacement WeakenCR(ParsedReplacement Input);

  ParsedReplacement WeakenDB(ParsedReplacement Input);

  ParsedReplacement WeakenOther(ParsedReplacement Input);

  ParsedReplacement ReducePCsToDF(ParsedReplacement Input);

  ParsedReplacement ReducePoison(ParsedReplacement Input);

  bool VerifyInput(ParsedReplacement &Input);

  bool safeToRemove(Inst *I, ParsedReplacement &Input);

  Inst *Eliminate(ParsedReplacement &Input, Inst *I);

  void ReduceRec(ParsedReplacement Input_, std::vector<ParsedReplacement> &Results);
  void Stats() {
    llvm::outs() << "Solver Calls: " << numSolverCalls << "\n";
  }
private:
  InstContext &IC;
  Solver *S;
  int varnum;
  int numSolverCalls;
  std::unordered_set<std::string> DNR;
};

}

#endif

