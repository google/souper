#include <souper/Infer/Preconditions.h>
#include <souper/Extractor/Solver.h>
#include <llvm/ADT/APInt.h>
#include <llvm/Support/CommandLine.h>
using llvm::APInt;

static llvm::cl::opt<bool> FixItNoVar("fixit-no-restrict-vars",
    llvm::cl::desc("Do not restrict input variables, only constants."
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> GenCR("gencr",
    llvm::cl::desc("Generate a CR precondition."
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> GenKB("genkb",
    llvm::cl::desc("Generate a KB precondition."
                   "(default=true)"),
llvm::cl::init(true));


namespace souper {

std::pair<std::vector<std::map<Inst *, llvm::KnownBits>>,
std::vector<std::map<Inst *, llvm::ConstantRange>>>
inferAbstractPreconditions(SynthesisContext &SC, Inst *RHS,
                               Solver *S, bool &FoundWeakest) {

  std::vector<std::map<Inst *, llvm::ConstantRange>> CRResults;
  std::vector<std::map<Inst *, llvm::KnownBits>> KBResults;
  if (GenKB) KBResults = inferAbstractKBPreconditions(SC, RHS, S, FoundWeakest);
  if (GenCR) CRResults = inferAbstractCRPreconditions(SC, RHS, S, FoundWeakest);
  return std::make_pair(KBResults, CRResults);
}

std::vector<std::map<Inst *, llvm::ConstantRange>>
  inferAbstractCRPreconditions(SynthesisContext &SC, Inst *RHS,
                               Solver *S, bool &FoundWeakest) {

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
    FoundWeakest = true;
    if (DebugLevel > 1) {
      llvm::errs() << "Already valid.\n";
    }
    return {};
  }

  std::vector<Inst *> Vars;
  findVars(Mapping.LHS, Vars);
  std::set<Inst *> FilteredVars;

  for (auto Var : Vars) {
    std::string NamePrefix = Var->Name;
    NamePrefix.resize(4);
    if (!FixItNoVar || Var->K != Inst::Var || NamePrefix == "fake") {
      FilteredVars.insert(Var);
    }
  }

  // std::map<Inst *, llvm::ConstantRange> OriginalState;

  // for (auto V : Vars) {
  //   OriginalState[V] = V->Range;
  // }

  std::vector<std::map<Inst *, llvm::ConstantRange>> Results;
  Inst *Precondition = SC.IC.getConst(llvm::APInt(1, true));

  std::vector<std::map<Inst *, llvm::APInt>> ValidInputs;
  // unlike for known bits, this is not guaranteed to terminate quickly.
  size_t Iterations = 0;
  while (true) {
    if (Iterations >= 10000) {
      break;
      // TODO adjust this threshold
    } else {
      Iterations++;
    }

    std::vector<Inst *> ModelInsts;
    std::vector<llvm::APInt> ModelVals;

    if (!ValidInputs.empty()) {
      auto LastInputSet = ValidInputs.back();
      // for (auto V : Vars) {
      //   V->KnownOnes = OriginalState[V].OriginalOne;
      //   V->KnownZeros = OriginalState[V].OriginalZero;
      // }
      Inst *NewPre = nullptr;
      for (size_t i = 0; i < Vars.size(); ++i) {
        auto &I = Vars[i];
        auto W = I->Width;
//          auto Zero = SC.IC.getConst(llvm::APInt(W, 0));

        auto VarConstraint = SC.IC.getInst(Inst::Ne, 1, {I, SC.IC.getConst(LastInputSet[I])});

        if (NewPre) {
          NewPre = SC.IC.getInst(Inst::Or, 1, {NewPre, VarConstraint});
        } else {
          NewPre = VarConstraint;
        }

      }

      // Do not find an input belonging to a derived abstract set.
      if (NewPre) {
        Precondition = SC.IC.getInst(Inst::And, 1, {Precondition, NewPre});
      }
    }

    // Find one input for which the given transformation is valid
    Models.clear();
    std::string Query = BuildQuery(SC.IC, SC.BPCs, PCCopy, Mapping,
                                   &ModelInsts, Precondition, true);


    S->isSatisfiable(Query, FoundWeakest, ModelInsts.size(),
                                       &ModelVals, SC.Timeout);

    std::map<Inst *, llvm::APInt> CurrentCE;
    if (FoundWeakest) {
      for (unsigned J = 0; J < ModelInsts.size(); ++J) {
        if (FilteredVars.find(ModelInsts[J]) != FilteredVars.end()) {
          CurrentCE[ModelInsts[J]] = ModelVals[J];
        } else {
          auto Zero = llvm::APInt(ModelInsts[J]->Width, 0);
          CurrentCE[ModelInsts[J]] = Zero;
        }

        if (DebugLevel >= 3) {
          llvm::outs() << "Starting with : " << ModelVals[J] << "\n";
        }
      }
      ValidInputs.push_back(CurrentCE);
    } else {
      if (ValidInputs.empty()) {
        if (DebugLevel >= 3) {
          llvm::outs() << "Transformation is not valid for any input.\n";
        }
        return {};
      } else {
        FoundWeakest = true;
        if (DebugLevel >= 3) {
          llvm::outs() << "Exhausted search space.\n";
        }
        break;
      }
    }

    // Widen CurrentCE into the largest possible CR which maintains validity
    // How to do this??




  }

  return {};
}

std::vector<std::map<Inst *, llvm::KnownBits>>
  inferAbstractKBPreconditions(SynthesisContext &SC, Inst *RHS,
                               Solver *S, bool &FoundWeakest) {
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
      FoundWeakest = true;
      if (DebugLevel > 1) {
        llvm::errs() << "Already valid.\n";
      }
      return {};
    }

    struct VarInfo {
      llvm::APInt OriginalZero;
      llvm::APInt OriginalOne;
    };

    std::vector<Inst *> Vars;
    findVars(Mapping.LHS, Vars);
    std::set<Inst *> FilteredVars;

    for (auto Var : Vars) {
      std::string NamePrefix = Var->Name;
      NamePrefix.resize(4);
      if (!FixItNoVar || Var->K != Inst::Var || NamePrefix == "fake") {
        FilteredVars.insert(Var);
      }
    }

    std::map<Inst *, VarInfo> OriginalState;

    for (auto V : Vars) {
      OriginalState[V].OriginalOne = V->KnownOnes;
      OriginalState[V].OriginalZero = V->KnownZeros;
    }

    std::vector<std::map<Inst *, llvm::KnownBits>> Results;
    Inst *Precondition = SC.IC.getConst(llvm::APInt(1, true));

    while (true) { // guaranteed to terminate in O(Width)
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
        Inst *NewPre = nullptr;
        for (size_t i = 0; i < Vars.size(); ++i) {
          auto &I = Vars[i];
          auto W = I->Width;
//          auto Zero = SC.IC.getConst(llvm::APInt(W, 0));
          auto AllOnes = SC.IC.getConst(llvm::APInt::getAllOnesValue(W));

          auto KnownOne = SC.IC.getConst(KB[I].One);
          auto KnownZero = SC.IC.getConst(KB[I].Zero);
          auto A = SC.IC.getInst(Inst::And, W, {I, KnownOne});

          auto B = SC.IC.getInst(Inst::And, W,
                     {SC.IC.getInst(Inst::Xor, W, {I, AllOnes}),
                      KnownZero});

          auto VarConstraint = SC.IC.getInst(Inst::Or, 1,
                       {SC.IC.getInst(Inst::Ne, 1, {A, KnownOne}),
                        SC.IC.getInst(Inst::Ne, 1, {B, KnownZero})});

          if (NewPre) {
            NewPre = SC.IC.getInst(Inst::Or, 1, {NewPre, VarConstraint});
          } else {
            NewPre = VarConstraint;
          }

        }
        // Do not find an input belonging to a derived abstract set.
        if (NewPre) {
          Precondition = SC.IC.getInst(Inst::And, 1, {Precondition, NewPre});
        }
      }

      // Find one input for which the given transformation is valid
      Models.clear();
      std::string Query = BuildQuery(SC.IC, SC.BPCs, PCCopy, Mapping,
                                     &ModelInsts, Precondition, true);


      S->isSatisfiable(Query, FoundWeakest, ModelInsts.size(),
                                         &ModelVals, SC.Timeout);

      std::map<Inst *, llvm::KnownBits> Known;
      if (FoundWeakest) {
        for (unsigned J = 0; J < ModelInsts.size(); ++J) {
          llvm::KnownBits KBCurrent(ModelInsts[J]->Width);
          if (FilteredVars.find(ModelInsts[J]) != FilteredVars.end()) {
            Known[ModelInsts[J]].One = ModelVals[J];
            Known[ModelInsts[J]].Zero = ~ModelVals[J];
          } else {
            auto Zero = llvm::APInt(ModelInsts[J]->Width, 0);
            Known[ModelInsts[J]].One = Zero;
            Known[ModelInsts[J]].Zero = Zero;
          }

          if (DebugLevel >= 3) {
            llvm::outs() << "Starting with : " << ModelVals[J] << "\n";
          }
        }
      } else {
        if (Results.empty()) {
          if (DebugLevel >= 3) {
            llvm::outs() << "Transformation is not valid for any input.\n";
          }
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
