#include <souper/Infer/Preconditions.h>
#include <souper/Extractor/Solver.h>
#include <llvm/ADT/APInt.h>
#include <llvm/Support/CommandLine.h>
using llvm::APInt;

namespace souper {
std::vector<std::map<Inst *, llvm::KnownBits>>
  inferAbstractKBPreconditions(SynthesisContext &SC, Inst *RHS,
                               SMTLIBSolver *SMTSolver, Solver *S, bool &FoundWeakest) {
    InstMapping Mapping(SC.LHS, RHS);
    bool Valid;
    if (DebugLevel >= 3) {
      PrintReplacement(llvm::outs(), SC.BPCs, SC.PCs, Mapping);
    }
    std::vector<std::pair<Inst *, APInt>> Models;

    if (std::error_code EC = S->isValid(SC.IC, SC.BPCs, SC.PCs, Mapping, Valid, &Models)) {
      llvm::errs() << EC.message() << '\n';
    }
    std::vector<InstMapping> PCCopy = SC.PCs;
    if (Valid) {
      llvm::outs() << "Already valid.\n";
      return {};
    }

    struct VarInfo {
      llvm::APInt OriginalZero;
      llvm::APInt OriginalOne;
    };

    std::vector<Inst *> Vars;
    findVars(Mapping.LHS, Vars);

    std::map<Inst *, VarInfo> OriginalState;

    for (auto V : Vars) {
      OriginalState[V].OriginalOne = V->KnownOnes;
      OriginalState[V].OriginalZero = V->KnownZeros;
    }

    std::vector<std::map<Inst *, llvm::KnownBits>> Results;
    Inst *Precondition = SC.IC.getConst(llvm::APInt(1, true));

    while (true) { // guaranteed to terminate
      if (!Results.empty()) {
        bool foundNonTop = false;;
        for (auto R : Results) {
          for (auto P : R) {
            if (!P.second.isUnknown()) {
              foundNonTop = true;
              break;
            }
          }
          if (!foundNonTop) {
            break;
          }
        }
        if (!foundNonTop) {
          break;
        }
      }

      std::vector<Inst *> ModelInsts;
      std::vector<llvm::APInt> ModelVals;

      if (!Results.empty()) {
        auto KB = Results.back();
        for (auto V : Vars) {
          V->KnownOnes = OriginalState[V].OriginalOne;
          V->KnownZeros = OriginalState[V].OriginalZero;
        }

        for (size_t i = 0; i < Vars.size(); ++i) {
          auto &I = Vars[i];
          auto W = I->Width;
          auto Zero = SC.IC.getConst(llvm::APInt(W, 0));
          auto AllOnes = SC.IC.getConst(llvm::APInt::getAllOnesValue(W));

          auto A = SC.IC.getInst(Inst::And, W, {I, SC.IC.getConst(KB[I].One)});

          auto B = SC.IC.getInst(Inst::And, W,
                     {SC.IC.getInst(Inst::Xor, W, {I, AllOnes}),
                      SC.IC.getConst(KB[I].Zero)});

          auto New = SC.IC.getInst(Inst::And, 1,
                       {SC.IC.getInst(Inst::Eq, 1, {A, Zero}),
                        SC.IC.getInst(Inst::Eq, 1, {B, Zero})});

          // Do not find an input belonging to a derived abstract set.
          Precondition = SC.IC.getInst(Inst::And, 1, {Precondition, New});
        }
      }

      Models.clear();
      std::string Query = BuildQuery(SC.IC, SC.BPCs, PCCopy, Mapping,
                                     &ModelInsts, Precondition, true);


      SMTSolver->isSatisfiable(Query, FoundWeakest, ModelInsts.size(),
                                         &ModelVals, SC.Timeout);

      std::map<Inst *, llvm::KnownBits> Known;
      if (FoundWeakest) {
        for (unsigned J = 0; J < ModelInsts.size(); ++J) {
          llvm::KnownBits KBCurrent(ModelInsts[J]->Width);
          Known[ModelInsts[J]].One = ModelVals[J];
          if (DebugLevel >= 3) {
            llvm::outs() << "Starting with : " << ModelVals[J] << "\n";
          }
          Known[ModelInsts[J]].Zero = ~ModelVals[J];
        }
      } else {
        if (Results.empty()) {
          llvm::outs() << "Transformation is not valid for any input.\n";
          return {};
        } else {
          FoundWeakest = true;
          if (DebugLevel >= 3) {
            llvm::outs() << "Exhausted search space.\n";
          }
          break;
        }
      }
      for (unsigned J = 0; J < Vars.size(); ++J) {
        Vars[J]->KnownZeros = Known[Vars[J]].Zero;
        Vars[J]->KnownOnes = Known[Vars[J]].One;
      }
      for (unsigned J = 0; J < Vars.size(); ++J) {
        auto W = Vars[J]->Width;
        for (unsigned I=0; I< W; I++) {
          if (Known[Vars[J]].Zero[I]) {
            APInt ZeroGuess = Known[Vars[J]].Zero & ~APInt::getOneBitSet(W, I);
            auto OldZero = Vars[J]->KnownZeros;
            Vars[J]->KnownZeros = ZeroGuess;
            Vars[J]->KnownOnes = Known[Vars[J]].One;
            if (DebugLevel >= 3)
              PrintReplacement(llvm::outs(), SC.BPCs, PCCopy, Mapping);

            Models.clear();
            if (std::error_code EC = S->isValid(SC.IC, SC.BPCs, PCCopy,
                                                Mapping, Valid, &Models)) {
              llvm::errs() << EC.message() << '\n';
            }

            if (Valid) {
              if (DebugLevel >= 3)
                llvm::outs() << "Valid\n";
              Known[Vars[J]].Zero = ZeroGuess;

            } else {
              Vars[J]->KnownZeros = OldZero;
              if (DebugLevel >= 3)
                llvm::outs() << "Invalid\n";
            }
          }

          if (Known[Vars[J]].One[I]) {
            APInt OneGuess = Known[Vars[J]].One & ~APInt::getOneBitSet(W, I);
            auto OldOne = Vars[J]->KnownOnes;
            Vars[J]->KnownZeros = Known[Vars[J]].Zero;
            Vars[J]->KnownOnes = OneGuess;

            if (DebugLevel >= 3)
              PrintReplacement(llvm::outs(), SC.BPCs, PCCopy, Mapping);

            Models.clear();
            if (std::error_code EC = S->isValid(SC.IC, SC.BPCs, PCCopy,
                                                Mapping, Valid, &Models)) {
              llvm::errs() << EC.message() << '\n';
            }

            if (Valid) {
              if (DebugLevel >= 3)
                llvm::outs() << "Valid\n";
              Known[Vars[J]].One = OneGuess;
            } else {
              Vars[J]->KnownOnes = OldOne;
              if (DebugLevel >= 3) {
                llvm::outs() << "Invalid\n";
              }
            }
          }
        }
      }
      if (DebugLevel >= 3) {
      for (auto P : Known) {
        llvm::outs() << "Derived : "
                     << Inst::getKnownBitsString(P.second.Zero, P.second.One) << "\t";
      }
      llvm::outs() << "\n";
      }
      Results.push_back(Known);
    }
    return Results;
  }
}
