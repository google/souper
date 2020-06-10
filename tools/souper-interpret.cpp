// Copyright 2014 The Souper Authors. All rights reserved.
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

#include "llvm/Support/KnownBits.h"
#include "llvm/Support/MemoryBuffer.h"

#include "souper/Infer/AbstractInterpreter.h"
#include "souper/Infer/Interpreter.h"
#include "souper/Parser/Parser.h"
#include "souper/Tool/GetSolver.h"
#include "souper/Util/LLVMUtils.h"

using namespace llvm;
using namespace souper;

unsigned DebugLevel;

static cl::opt<unsigned, /*ExternalStorage=*/true>
DebugFlagParser("souper-debug-level",
     cl::desc("Control the verbose level of debug output (default=1). "
     "The larger the number is, the more fine-grained debug "
     "information will be printed."),
     cl::location(DebugLevel), cl::init(1));

static cl::list<std::string>
InputValueStrings("input-values", cl::desc("<input values>"),
                  cl::CommaSeparated);

static cl::opt<std::string>
InputFilename(cl::Positional, cl::desc("<input souper optimization>"),
              cl::init("-"));

namespace {
  enum class CompareDataflowResult {
    SAME,
    LESS,
    GREATER,
    INCOMPARABLE
  };

  CompareDataflowResult compareKnownBits(KnownBits &a, KnownBits &b) {
    assert(!a.hasConflict() && !b.hasConflict() && "Can't compare conflicting KnownBits operands!");

    APInt unknownA = ~(a.One ^ a.Zero);
    APInt unknownB = ~(b.One ^ b.Zero);

    // report if there's any conflict in known bits
    if (!((a.Zero & b.One).isNullValue()) || !((a.One & b.Zero).isNullValue()))
      return CompareDataflowResult::INCOMPARABLE;

    if ((unknownA | unknownB).countPopulation() >
	std::max(unknownA.countPopulation(), unknownB.countPopulation()))
      return CompareDataflowResult::INCOMPARABLE;
    else if (unknownA.countPopulation() > unknownB.countPopulation())
      return CompareDataflowResult::LESS;
    else if (unknownA.countPopulation() < unknownB.countPopulation())
      return CompareDataflowResult::GREATER;
    else
      return CompareDataflowResult::SAME;
  }
} // anonymous

// TODO could support poison, undef, and UB here
static bool parseInput(const std::string &S, std::string &Name,
                       std::string &Val) {
  if (S[0] != '%')
    return false;
  int Pos = 1;
  while (Pos < S.size() &&
         ((S[Pos] >= '0' && S[Pos] <= '9') ||
	  (S[Pos] >= 'a' && S[Pos] <= 'z') ||
	  (S[Pos] >= 'A' && S[Pos] <= 'Z')))
    Pos++;
  if (Pos == 1 || Pos == S.size())
    return false;
  Name = S.substr(1, Pos - 1);
  if (S[Pos] != '=')
    return false;
  Pos++;
  if (Pos >= S.size())
    return false;
  int StartVal = Pos;
  while (Pos < S.size()) {
    if ((S[Pos] < '0' || S[Pos] > '9') && S[Pos] != '-')
      return false;
    Pos++;
  }
  Val = S.substr(StartVal, Pos - StartVal);
  return true;
}

static bool fitsInBits(std::string str, unsigned bits) {
  auto I = APInt(bits, str, 10);
  std::string S = I.toString(10, true);
  std::string U = I.toString(10, false);
  return (str.compare(S) == 0) || (str.compare(U) == 0);
}

static int Interpret(const MemoryBufferRef &MB, Solver *S) {
  InstContext IC;
  std::string ErrStr;
  std::vector<ParsedReplacement> Reps;
  std::vector<ReplacementContext> Contexts;
  Reps = ParseReplacementLHSs(IC, MB.getBufferIdentifier(), MB.getBuffer(),
			      Contexts, ErrStr);
  if (!ErrStr.empty()) {
    llvm::errs() << ErrStr << '\n';
    return 1;
  }

  unsigned Index = 0;
  int Ret = 0;
  int Success = 0, Fail = 0, Error = 0;
  for (unsigned i = 0 ; i < Reps.size(); ++i) {

    std::vector<Inst *> Vars;
    findVars(Reps[i].Mapping.LHS, Vars);

    if (InputValueStrings.size() < Vars.size()) {
      llvm::outs() << "Warning: not enough inputs.\n";
      llvm::outs() << "(Feature, not bug) Running abstract interpreters (without input specialization).\n";

      ConcreteInterpreter BlankCI;

      auto KB = KnownBitsAnalysis().findKnownBits(Reps[i].Mapping.LHS, BlankCI, false);
      llvm::outs() << "KnownBits result: \n"
                   << KnownBitsAnalysis::knownBitsString(KB) << '\n';

      auto CR = ConstantRangeAnalysis().findConstantRange(Reps[i].Mapping.LHS, BlankCI, false);

      llvm::outs() << "Constant Range result: " << CR << "\n";

      auto RB = RestrictedBitsAnalysis().findRestrictedBits(Reps[i].Mapping.LHS);
      llvm::outs() << "Restricted Bits result: " << RB.toString(2, false) << "\n";


      auto PrintDB = [](std::string Preamble, auto DB) {
        llvm::outs() << Preamble << "\n";
        std::vector<std::pair<Inst *, llvm::APInt>> Copy(DB.size());
        std::copy(DB.begin(), DB.end(), Copy.begin());
        std::sort(Copy.begin(), Copy.end(),
                  [] (std::pair<Inst *, llvm::APInt> A, std::pair<Inst *, llvm::APInt> B) {
                    return A.first->Name < B.first->Name;
                  });
        for (auto P : Copy) {
          llvm::outs() << "var : " << P.first->Name << '\t' << souper::getPaddedBinaryString(P.second) << "\n";
        }
        llvm::outs() << "=====\n";
      };

      auto MustDB = MustDemandedBitsAnalysis().findMustDemandedBits(Reps[i].Mapping.LHS);
      PrintDB("MustDemandedBitsAnalysis result:", MustDB);

      auto DCBits = DontCareBitsAnalysis().findDontCareBits(Reps[i].Mapping.LHS);
      PrintDB("DontCareBitsAnalysis result:", DCBits);

      return 0;
    }

    ValueCache InputValues;
    std::vector<InstMapping> InputValuesInstMappings;
    BlockCache BlockSet;
    for (auto S : InputValueStrings) {
      std::string Name, Val;
      if (!parseInput(S, Name, Val)) {
        llvm::errs() << "Error: Cannot parse input value '" << S << "'\n";
        return 1;
      }
      bool Found = false;
      for (auto V : Vars) {
        if (V->Name == Name) {
          if (!fitsInBits(Val, V->Width)) {
            llvm::errs() << "Error: value '" << Val << "' is too large for ";
            llvm::errs() << V->Width << " bits.\n";
            return 1;
          }
          if (InputValues.find(V) != InputValues.end()) {
            llvm::errs() << "Error: duplicate value for %" << V->Name << "\n";
            return 1;
          }
          APInt ValObj = APInt(V->Width, Val, 10);
          InputValues[V] = ValObj;
          InputValuesInstMappings.emplace_back(V, IC.getConst(ValObj));
          Found = true;
          if (DebugLevel > 3)
            llvm::outs() << "var '" << V->Name << "' gets value '" <<
              APInt(V->Width, Val, 10) << "'\n";
          break;
        }
      }
      if (auto B = Contexts[i].getBlock(Name)) {
        unsigned BlockValue = std::stoul(Val);
        if (BlockValue  >= B->Preds) {
          llvm::errs() << "Error: value '" << Val << "' is to large for block %";
          llvm::errs() << B->Number << "\n";
          return 1;
        }
        if (BlockSet.find(B) != BlockSet.end()) {
          llvm::errs() << "Error: duplicate value for %" << B->Number << "\n";
          return 1;
        }
        BlockSet.insert(B);
        B->ConcretePred = BlockValue;
        Found = true;
        if (DebugLevel > 3) {
          llvm::outs() << "block '" << B->Number << "' gets value '" << Val << "'\n";
        }
      }
      if (!Found) {
        llvm::errs() << "No var with name '" << Name << "' found.\n";
        return 1;
      }
    }

    ConcreteInterpreter CI(InputValues);
    // Concrete Interpreter
    if (isConcrete(Reps[i].Mapping.LHS)) {
      llvm::outs() << " -------- Concrete Interpreter ----------- \n";
      CI = ConcreteInterpreter(Reps[i].Mapping.LHS, InputValues);
      auto Res = CI.evaluateInst(Reps[i].Mapping.LHS);
      Res.print(llvm::outs());
      llvm::outs() << "\n";
    }

    // Known bits interpreter
    llvm::outs() << "\n -------- KnownBits Interpreter ----------- \n";
    auto KB = KnownBitsAnalysis().findKnownBits(Reps[i].Mapping.LHS, CI);
    auto KBSolver = KnownBitsAnalysis::findKnownBitsUsingSolver(Reps[i].Mapping.LHS, S, InputValuesInstMappings);
    llvm::outs() << "KnownBits result: \n" << KnownBitsAnalysis::knownBitsString(KB) << '\n';
    llvm::outs() << "KnownBits result using solver: \n" << KnownBitsAnalysis::knownBitsString(KBSolver) << "\n\n";

    switch(compareKnownBits(KB, KBSolver)) {
    case CompareDataflowResult::SAME:
      llvm::outs() << "Same precision.\n";
      break;
    case CompareDataflowResult::LESS:
      llvm::outs() << "Dataflow is less precise than solver.\n";
      break;
    case CompareDataflowResult::GREATER:
      llvm::outs() << "Dataflow is more precise than solver.\n";
      break;
    case CompareDataflowResult::INCOMPARABLE:
      llvm::outs() << "Reults are incomparable.\n";
      break;
    }

    // Constant Ranges interpreter
    llvm::outs() << "\n -------- ConstantRanges Interpreter ----------- \n";
    auto CR = ConstantRangeAnalysis().findConstantRange(Reps[i].Mapping.LHS, CI);
    auto CRSolver = ConstantRangeAnalysis::findConstantRangeUsingSolver(Reps[i].Mapping.LHS, S, InputValuesInstMappings);
    llvm::outs() << "ConstantRange result: \n" << CR << '\n';
    llvm::outs() << "ConstantRange result using solver: \n" << CRSolver << "\n\n";

    if (CR == CRSolver)
      llvm::outs() << "Same precision.\n";
    else if (CR.contains(CRSolver))
      llvm::outs() << "Dataflow is less precise than solver.\n";
    else if (CRSolver.contains(CR))
      llvm::outs() << "Dataflow is more precise than solver.\n";
    else
      llvm::outs() << "Reults are incomparable.\n";


    Index++;
  }

  return Ret;
}

int main(int argc, char **argv) {
  cl::ParseCommandLineOptions(argc, argv);
  KVStore *KV = 0;
  std::unique_ptr<Solver> S = 0;
  S = GetSolver(KV);

  auto MB = MemoryBuffer::getFileOrSTDIN(InputFilename);
  if (!MB) {
    llvm::errs() << MB.getError().message() << '\n';
    return 1;
  }
  return Interpret((*MB)->getMemBufferRef(), S.get());
}
