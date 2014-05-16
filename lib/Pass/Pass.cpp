#include "souper/SMTLIB2/Solver.h"
#include "souper/Tool/GetSolverFromArgs.h"
#include "souper/Tool/CandidateMapUtils.h"
#include "llvm/Pass.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace souper;
using namespace llvm;

namespace {
static cl::opt<bool> DebugSouperPass("debug-souper", cl::Hidden,
                                     cl::init(false), cl::desc("Debug Souper"));

struct SouperPass : public FunctionPass {
  static char ID;
  std::unique_ptr<SMTLIBSolver> Solver;

public:
  SouperPass() : FunctionPass(ID), Solver(GetSolverFromArgs()) {}

  void getAnalysisUsage(AnalysisUsage &Info) const {
    Info.addRequired<LoopInfo>();
  }

  bool runOnFunction(Function &F) {
    bool changed = false;
    InstContext IC;
    ExprBuilderContext EBC;
    CandidateMap CandMap;

    LoopInfo *LI = &getAnalysis<LoopInfo>();

    FunctionCandidateSet CS = ExtractCandidatesFromPass(&F, LI, IC, EBC);
    for (auto &B : CS.Blocks) {
      for (auto &R : B->Replacements) {
        AddToCandidateMap(CandMap, R);
      }
    }

    if (Solver) {
      if (DebugSouperPass) {
        errs() << "; Listing valid replacements.\n";
        errs() << "; Using solver: " << Solver->getName() << '\n';
      }

      for (const auto &Cand : CandMap) {
        bool Sat;
        if (llvm::error_code EC =
                Solver->isSatisfiable(Cand.second.getQuery(), Sat)) {
          llvm::errs() << "Unable to query solver: " << EC.message() << '\n';
          return changed;
        }

        if (!Sat) {
          if (DebugSouperPass) {
            errs() << '\n';
            Cand.second.print(errs());
          }

          // We are assuming that at most one candidate applies per instruction.
          // If not, then the instruction will already have been replaced and
          // this code will crash badly.
          for (auto *O : Cand.second.Origins) {
            BasicBlock *BB = O->getParent();
            BasicBlock::iterator BI = BB->begin();
            while (&(*BI) != O)
              BI++;
            Constant *CI = ConstantInt::get(
                O->getType(), Cand.second.Mapping.Replacement->Val);
            if (DebugSouperPass) {
              errs() << "; Replacing \"";
              O->print(errs());
              errs() << "\" with \"";
              CI->print(errs());
              errs() << "\"\n";
            }
            ReplaceInstWithValue(BB->getInstList(), BI, CI);
            changed = true;
          }
        }
      }
    } else if (DebugSouperPass) {
      errs() << "; No solver specified; listing all candidate replacements.\n";
      for (const auto &Cand : CandMap) {
        errs() << '\n';
        Cand.second.print(errs());
      }
    }

    return changed;
  }
};

char SouperPass::ID = 0;
}

static RegisterPass<SouperPass> X("souper", "Souper super-optimizer pass",
                                  false, false);
