#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/KnownBits.h"

#include "souper/Infer/Preconditions.h"

#include "souper/Inst/InstGraph.h"
#include "souper/Parser/Parser.h"
#include "souper/Tool/GetSolver.h"
#include "souper/Util/DfaUtils.h"

using namespace llvm;
using namespace souper;

unsigned DebugLevel;

static cl::opt<unsigned, /*ExternalStorage=*/true>
DebugFlagParser("souper-debug-level",
     cl::desc("Control the verbose level of debug output (default=1). "
     "The larger the number is, the more fine-grained debug "
     "information will be printed."),
     cl::location(DebugLevel), cl::init(1));

static cl::opt<std::string>
InputFilename(cl::Positional, cl::desc("<input souper optimization>"),
              cl::init("-"));

static llvm::cl::opt<bool> RemoveLeaf("remove-leaf",
    llvm::cl::desc("Try to generalize a valid optimization by replacing"
                   "the use of a once-used variable with a new variable"
                   "(default=false)"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> FixIt("fixit",
    llvm::cl::desc("Given an invalid optimization, generate a valid one."
                   "(default=false)"),
    llvm::cl::init(false));

void Generalize(InstContext &IC, Solver *S, ParsedReplacement Input) {
  bool FoundWP = false;
  std::vector<std::map<Inst *, llvm::KnownBits>> Results;
  S->abstractPrecondition(Input.BPCs, Input.PCs, Input.Mapping, IC, FoundWP, Results);

  if (FoundWP && Results.empty()) {
    Input.print(llvm::outs(), true);
  } else {
    for (auto &&Result : Results) { // Each result is a disjunction
      for (auto Pair: Result) {
        Pair.first->KnownOnes = Pair.second.One;
        Pair.first->KnownZeros = Pair.second.Zero;
      }
      Input.print(llvm::outs(), true);
    }
  }
}

// TODO: Return modified instructions instead of just printing out
void RemoveLeafAndGeneralize(InstContext &IC,
                               Solver *S, ParsedReplacement Input) {

  if (DebugLevel > 1) {
  llvm::errs() << "Attempting to generalize by removing leaf.\n";
  }
  // TODO: Do not generalize by removing leaf if LHS has one inst.

  std::map<Inst *, std::set<Inst *>> Uses;

  std::vector<Inst *> Stack{Input.Mapping.LHS, Input.Mapping.RHS};
  // TODO: Find uses in PCs/BPCs

  std::set<Inst *> Visited;
  while (!Stack.empty()) {
   auto Current = Stack.back();
   Stack.pop_back();
   Visited.insert(Current);

   for (auto Op : Current->Ops) {
     if (Op->K == Inst::Var) {
       Uses[Op].insert(Current);
       // Intentionally skips root
     }
     if (Visited.find(Op) == Visited.end()) {
       Stack.push_back(Op);
     }
   }
  }

  // Find a variable with one use;
  Inst *UsedOnce = nullptr;
  for (auto P : Uses) {
    if (P.second.size() == 1) {
      UsedOnce = P.first;
      break;
    }
  }

  if (!UsedOnce) {
    llvm::outs() << "Failed. No var with one use.";
    return;
  } else {
    Inst *User = *Uses[UsedOnce].begin();
    Inst *NewVar = IC.createVar(User->Width, "newvar");

    std::map<Inst *, Inst *> ICache;
    ICache[User] = NewVar;

    std::map<Block *, Block *> BCache;
    std::map<Inst *, llvm::APInt> CMap;

    Input.Mapping.LHS = getInstCopy(Input.Mapping.LHS, IC, ICache,
                                    BCache, &CMap, false);

    Input.Mapping.RHS = getInstCopy(Input.Mapping.RHS, IC, ICache,
                                    BCache, &CMap, false);

    // TODO: Replace PCs/BPCs
  }

  Generalize(IC, S, Input);
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

  InstContext IC;
  std::string ErrStr;

  auto &&Data = (*MB)->getMemBufferRef();
  auto Inputs = ParseReplacements(IC, Data.getBufferIdentifier(),
                                  Data.getBuffer(), ErrStr);

  if (!ErrStr.empty()) {
    llvm::errs() << ErrStr << '\n';
    return 1;
  }

  // TODO: Write default action which chooses what to do based on input structure

  for (auto &&Input: Inputs) {
    if (FixIt) {
      // TODO: Verify that inputs are valid optimizations
      Generalize(IC, S.get(), Input);
    }
    if (RemoveLeaf) {
      RemoveLeafAndGeneralize(IC, S.get(), Input);
    }
    // if (EviscerateRoot) {...}
    // if (SymbolizeConstant) {...}
    // if (LiberateWidth) {...}
  }

  return 0;
}
