#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/KnownBits.h"

#include "souper/Infer/Preconditions.h"
#include "souper/Infer/EnumerativeSynthesis.h"
#include "souper/Infer/ConstantSynthesis.h"
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

static llvm::cl::opt<bool> IgnorePCs("ignore-pcs",
    llvm::cl::desc("Ignore inputs which have souper path conditions."
                   "(default=false)"),
    llvm::cl::init(false));

static const std::map<Inst::Kind, std::string> MatchOps = {
  {Inst::Add, "m_Add("}, {Inst::Sub, "m_Sub("},
  {Inst::Mul, "m_Mul("},

  {Inst::Shl, "m_Shl("}, {Inst::LShr, "m_LShr("},
  {Inst::AShr, "m_AShr("},

  {Inst::AddNSW, "m_NSWAdd("}, {Inst::SubNSW, "m_NSWSub("},
  {Inst::MulNSW, "m_NSWMul("}, {Inst::ShlNSW, "m_NSWShl("},

  {Inst::AddNUW, "m_NUWAdd("}, {Inst::SubNUW, "m_NUWSub("},
  {Inst::MulNUW, "m_NUWMul("}, {Inst::ShlNUW, "m_NUWShl("},

  {Inst::SDiv, "m_SDiv("}, {Inst::UDiv, "m_UDiv("},
  {Inst::SRem, "m_SRem("}, {Inst::URem, "m_URem("},


  {Inst::And, "m_And("}, {Inst::Or, "m_Or("},
  {Inst::Xor, "m_Xor("},

  {Inst::Eq, "m_Cmp("},
  {Inst::Ne, "m_Cmp("},
  {Inst::Ule, "m_Cmp("},
  {Inst::Ult, "m_Cmp("},
  {Inst::Sle, "m_Cmp("},
  {Inst::Slt, "m_Cmp("},

  {Inst::SExt, "m_SExt("},
  {Inst::ZExt, "m_ZExt("},
  {Inst::Trunc, "m_Trunc("},
  {Inst::Select, "m_Select("},
};

static const std::map<Inst::Kind, std::string> CreateOps = {
  {Inst::Shl, "CreateShl("}, {Inst::AShr, "CreateAShr("}, {Inst::LShr, "CreateLShr("},
  {Inst::Add, "CreateAdd("}, {Inst::Mul, "CreateMul("}, {Inst::Sub, "CreateSub("},
  {Inst::SDiv, "CreateSDiv("}, {Inst::UDiv, "CreateUDiv("}, {Inst::SRem, "CreateSRem("},
  {Inst::URem, "CreateURem("},
  {Inst::Or, "CreateOr("}, {Inst::And, "CreateAnd("}, {Inst::Xor, "CreateXor("},

  {Inst::Eq, "CreateCmp(ICmpInst::ICMP_EQ, "},
  {Inst::Ne, "CreateCmp(ICmpInst::ICMP_NE, "},
  {Inst::Ule, "CreateCmp(ICmpInst::ICMP_ULE, "},
  {Inst::Ult, "CreateCmp(ICmpInst::ICMP_ULT, "},
  {Inst::Sle, "CreateCmp(ICmpInst::ICMP_SLE, "},
  {Inst::Slt, "CreateCmp(ICmpInst::ICMP_SLT, "},

//  {Inst::Trunc, "CreateTrunc("},
//  {Inst::SExt, "CreateSExt("},
//  {Inst::ZExt, "CreateZExt("},
  // TODO have to create dest type

  {Inst::Select, "CreateSelect("},

  {Inst::Const, "dummy"},
};

static const std::map<Inst::Kind, std::string> PredNames = {
  {Inst::Eq, "ICmpInst::ICMP_EQ"},
  {Inst::Ne, "ICmpInst::ICMP_NE"},
  {Inst::Ule, "ICmpInst::ICMP_ULE"},
  {Inst::Ult, "ICmpInst::ICMP_ULT"},
  {Inst::Sle, "ICmpInst::ICMP_SLE"},
  {Inst::Slt, "ICmpInst::ICMP_SLT"},
};

struct SymbolTable : public std::map<Inst *, std::vector<std::string>> {
  std::vector<std::pair<std::string, std::string>> Eqs;
  std::map<Inst *, std::string> Preds;
  std::vector<Inst *> Vars;

  void RegisterPred(Inst *I) {
    if (PredNames.find(I->K) == PredNames.end()) {
      return;
    }
    auto Name = "P" + std::to_string(Preds.size());
    Preds[I] = Name;
    Eqs.push_back(std::make_pair(Name, PredNames.at(I->K)));
  }
  template<typename Stream>
  void PrintPreds(Stream &Out) {
    if (Preds.empty()) {
      return;
    }
    Out << "ICmpInst::Predicate ";
    bool first = true;
    for (auto &&P : Preds) {
      if (first) {
        first = false;
      } else {
        Out << ", ";
      }
      Out << P.second;
    }
    Out << ";\n";
  }
  void GenVarEqConstraints() {
    for (auto &&S : *this) {
      if (S.second.size() > 1) {
        for (int i = 1; i < S.second.size(); ++i) {
          Eqs.push_back(std::make_pair(S.second[0], S.second[i]));
        }
      }
    }
  }
  template <typename Stream>
  void PrintEqPre(Stream &Out) {
    if (Eqs.empty()) {
      return;
    }
    Out << "if (";
    bool first = true;
    for (auto &&P : Eqs) {
      if (first) {
        first = false;
      } else {
        Out << " && ";
      }
      Out << P.first << " == " << P.second;
    }
    Out << ") {\n";
  }
  template <typename Stream>
  void PrintEqPost(Stream &Out) {
    if (Eqs.empty()) {
      return;
    }
    Out << "}\n";
  }

  template <typename Stream>
  void PrintWidthPre(Inst *LHS, Stream &Out) {
    findVars(LHS, Vars);
    if (Vars.empty()) {
      return;
    }
    Out << "if (util::check_width({";
    bool first = true;
    for (auto V : Vars) {
      if (first) {
        first = false;
      } else {
        Out << ", ";
      }
      Out << this->at(V)[0];
    }
    Out << "}, {";
    first = true;
    for (auto V : Vars) {
      if (first) {
        first = false;
      } else {
        Out << ", ";
      }
      Out << V->Width;
    }
    Out << "})) {\n";
  }
  template <typename Stream>
  void PrintWidthPost(Stream &Out) {
    if (Vars.empty()) {
      return;
    }
    Out << "\n}\n";
  }
};


//TODO: Enforce bitwidth
template <typename Stream>
bool GenLHSMatcher(Inst *I, Stream &Out, SymbolTable &Syms) {
  auto It = MatchOps.find(I->K);
  if (It == MatchOps.end()) {
    llvm::errs() << "\nUnimplemented matcher:" << Inst::getKindName(I->K) << "\n";
    return false;
  }
  auto Op = It->second;

  Out << Op;

  if (PredNames.find(I->K) != PredNames.end()) {
    Out << Syms.Preds[I] << ", ";
  }

  bool first = true;
  for (auto Child : I->Ops) {
    if (first) {
      first = false;
    } else {
      Out << ", ";
    }
    if (Child->K == Inst::Const) {
      Out << "m_SpecificInt(" << Child->Val << ")";
    } else if (Child->K == Inst::Var) {
      Out << "m_Value(" << Syms[Child].back() << ")";
      Syms[Child].pop_back();
    } else {
      if (!GenLHSMatcher(Child, Out, Syms)) {
        return false;
      }
    }

  }
  Out << ")";
  return true;
}

template <typename Stream>
bool GenRHSCreator(Inst *I, Stream &Out, SymbolTable &Syms) {
  auto It = CreateOps.find(I->K);
  if (It == CreateOps.end()) {
    llvm::errs() << "\nUnimplemented creator:" << Inst::getKindName(I->K) << "\n";
    return false;
  }
  auto Op = It->second;

  Out << Op;
  bool first = true;
  for (auto Child : I->Ops) {
    if (first) {
      first = false;
    } else {
      Out << ", ";
    }
    if (Syms.find(Child) != Syms.end()) {
      Out << Syms[Child][0];
    } else {
      if (!GenRHSCreator(Child, Out, Syms)) {
        return false;
      }
    }

  }
  if (I->K == Inst::Trunc || I->K == Inst::SExt || I->K == Inst::ZExt) {
    Out << ", T(" << I->Width << ")";
  }
  Out << ")";

  return true;
}

template <typename Stream, typename Container>
void printPath(Stream &Out, const Container& C) {
  Out << "{";
  bool first = true;
  for (auto c : C) {
    if (first) {
      first = false;
    } else {
      Out << ", ";
    }
    Out << c;
  }
  Out << "}";
}

template <typename Stream>
bool InitSymbolTable(Inst *Root, Inst *RHS, Stream &Out, SymbolTable &Syms) {
  std::map<Inst *, std::vector<int>> Paths;

  std::set<Inst *> Consts;


  Paths[Root] = {}; // root has an empty path
  std::vector<Inst *> Stack{Root};

  int varnum = 0;
  while (!Stack.empty()) {
    auto I = Stack.back();
    Stack.pop_back();
    Syms.RegisterPred(I);
    if (I->K == Inst::Var) {
      Syms[I].push_back("x" + std::to_string(varnum++));
    }
    if (I->K == Inst::Const) {
      Consts.insert(I);
    }
    for (int i = 0; i < I->Ops.size(); ++i) {
      Paths[I->Ops[i]] = Paths[I]; // Child inherits parent's path
      Paths[I->Ops[i]].push_back(i);
      Stack.push_back(I->Ops[i]); // Souper exprs are DAGs
    }
  }


  std::set<Inst *> LHSRefs;
  std::set<Inst *> Visited;
  std::set<Inst *> ConstRefs;
  Stack.push_back(RHS);
  while (!Stack.empty()) {
    auto I = Stack.back();
    Stack.pop_back();
    Visited.insert(I);
    if (I->K == Inst::Const) {
      Consts.insert(I);
      ConstRefs.insert(I);
    }
    if (Paths.find(I) != Paths.end()) {
      LHSRefs.insert(I);
    } else {
      for (auto Child : I->Ops) {
        if (Visited.find(Child) == Visited.end()) {
          Stack.push_back(Child);
        }
      }
    }
  }

  if (!Syms.empty()) {
    Out << "llvm::Value ";
    bool first = true;
    for (auto &&S : Syms) {
      for (auto &&Name : S.second) {
        if (first) {
          first = false;
        } else {
          Out << ", ";
        }
        Out << "*" << Name;
      }
    }
    Out << ";\n";
  }

  varnum = 0;
  for (auto &&P : Paths) {
    if (P.first == Root || P.first->K == Inst::Var
        || LHSRefs.find(P.first) == LHSRefs.end()) {
      continue;
    }
//    std::string Name = "I";
//    for (auto idx : P.second) {
//      auto NewName = "y" + std::to_string(varnum++);
//      Out << "auto " << NewName << " = cast<Instruction>(" << Name;
//      Out << ")->getOperand(" << idx << ");\n";
//      std::swap(Name, NewName);
//    }
//    Syms[P.first].push_back(Name);

    auto Name = "y" + std::to_string(varnum++);
    Out << "auto " << Name << " = util::node(I, ";
    printPath(Out, P.second);
    Out << ");\n";
    Syms[P.first].push_back(Name);
  }
  Syms[Root].push_back("I");

  varnum = 0;
  for (auto C : Consts) {
    if (ConstRefs.find(C) == ConstRefs.end()) {
      continue;
    }
    auto Name = "C" + std::to_string(varnum++);
    Out << "auto " << Name << " = C("
        << C->Val.getBitWidth() <<", "
        << C->Val << ");\n";
    Syms[C].push_back(Name);
  }
  Syms.PrintPreds(Out);
  return true;
}

template <typename Stream>
bool GenMatcher(ParsedReplacement Input, Stream &Out, size_t OptID) {
  SymbolTable Syms;
  Out << "{\n";

  if (!InitSymbolTable(Input.Mapping.LHS, Input.Mapping.RHS, Out, Syms)) {
    return false;
  }

  Out << "if (match(I, ";

  SymbolTable SymsCopy = Syms;
  if (!GenLHSMatcher(Input.Mapping.LHS, Out, SymsCopy)) {
    return false;
  }
  Out << ")) {\n";

  Syms.GenVarEqConstraints();
  Syms.PrintEqPre(Out);
  Syms.PrintWidthPre(Input.Mapping.LHS, Out);
  Out << "  St.hit(" << OptID << ");\n";
  if (Syms.find(Input.Mapping.RHS) != Syms.end()) {
    Out << "  return " << Syms[Input.Mapping.RHS][0] << ";";
  } else if (Input.Mapping.RHS->K == Inst::Const) {
    Out << "  APInt Result("
        << Input.Mapping.RHS->Width <<", "
        << Input.Mapping.RHS->Val << ");\n";
    Out << "  return ConstantInt::get(TheContext, Result);";
  } else {
    Out << "  return B->";
    if (!GenRHSCreator(Input.Mapping.RHS, Out, Syms)) {
      return false;
    }
    Out << ";";
  }
  Out << "\n}\n}";

  Syms.PrintWidthPost(Out);
  Syms.PrintEqPost(Out);

  return true;
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

  size_t optnumber = 0;
  for (auto &&Input: Inputs) {
    if (IgnorePCs && !Input.PCs.empty()) {
      continue;
    }

    std::string Str;
    llvm::raw_string_ostream Out(Str);
    if (GenMatcher(Input, Out, optnumber)) {
      auto current = optnumber++;
      llvm::outs() << "/* Opt : " << current << "\n";
      Input.print(llvm::outs(), true);
      llvm::outs() << "*/\n";
      llvm::outs() << Str << "\n";
      llvm::outs().flush();
    } else {
      llvm::errs() << "Failed to generate matcher.\n\n\n";
      llvm::errs().flush();
    }
  }

  return 0;
}
