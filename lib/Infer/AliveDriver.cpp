#include "souper/Extractor/ExprBuilder.h"
#include "souper/Infer/AliveDriver.h"
#include "souper/Inst/Inst.h"

#include "alive2/ir/constant.h"
#include "alive2/ir/function.h"
#include "alive2/ir/instr.h"
#include "alive2/ir/state.h"
#include "alive2/smt/ctx.h"
#include "alive2/smt/expr.h"
#include "alive2/smt/smt.h"
#include "alive2/smt/solver.h"
#include "alive2/tools/transform.h"
#include "alive2/util/config.h"
#include "alive2/util/errors.h"
#include "alive2/util/symexec.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"

#include <iostream>
#include <memory>
#include <set>
#include <sstream>
#include <string_view>
#include <unordered_set>
#include <z3.h>

extern unsigned DebugLevel;
static const int MaxTries = 30;

bool startsWith(const std::string &pre, const std::string &str) {
  return std::equal(pre.begin(), pre.end(), str.begin());
}

namespace {

static llvm::cl::opt<bool> DisableUndefInput("alive-disable-undef-input",
  llvm::cl::desc("Assume inputs can not be undef (default = false)"),
  llvm::cl::init(false));

static llvm::cl::opt<bool> SkipAliveSolver("alive-skip-solver",
  llvm::cl::desc("Omit Alive solver calls for performance testing (default = false)"),
  llvm::cl::init(false));

class FunctionBuilder {
public:
  FunctionBuilder(IR::Function &F_) : F(F_) {}

  template <typename A>
  IR::Value *freeze(IR::Type &t, std::string name, A a) {
    return append
      (std::make_unique<IR::Freeze>(t, std::move(name), *toValue(t, a)));
  }

  IR::Value *undef(IR::Type &t, std::string name) {
    auto undef = std::make_unique<IR::UndefValue>(t);
    auto undef_ptr = undef.get();
    F.addUndef(std::move(undef));
    return append
      (std::make_unique<IR::UnaryOp>(t, std::move(name),
        *undef_ptr, IR::UnaryOp::Copy));
  }

  template <typename A, typename B, typename ...Others>
  IR::Value *binOp(IR::Type &t, std::string name, A a, B b,Others... others) {
    return append
      (std::make_unique<IR::BinOp>
      (t, std::move(name), *toValue(t, a), *toValue(t, b), others...));
  }

  template <typename A, typename B, typename C, typename ...Others>
  IR::Value *ternaryOp(IR::Type &t, std::string name, A a, B b, C c,
                       Others... others) {
    return append
      (std::make_unique<IR::TernaryOp>
      (t, std::move(name), *toValue(t, a), *toValue(t, b), *toValue(t, c),
       others...));
  }

  template <typename A>
  IR::Value *conversionOp(IR::Type &t, std::string name, A a,
                          IR::ConversionOp::Op Op) {
    return append
      (std::make_unique<IR::ConversionOp>
      (t, std::move(name), *toValue(t, a), Op));
  }

  template <typename A, typename B, typename C>
  IR::Value *select(IR::Type &t, std::string name, A a, B b, C c) {
    return append
      (std::make_unique<IR::Select>
      (t, std::move(name), *toValue(t, a), *toValue(t, b), *toValue(t, c)));
  }

  template <typename A, typename B>
  IR::Value *iCmp(IR::Type &t, std::string name,
                  IR::ICmp::Cond cond, A a, B b) {
    return append
      (std::make_unique<IR::ICmp>
      (t, std::move(name), cond, *toValue(t, a), *toValue(t, b)));
  }

  IR::Value *extractvalue(IR::Type &t, std::string name, IR::Value *a, unsigned idx) {
    auto p = std::make_unique<IR::ExtractValue>(t, std::move(name), *toValue(t, a));
    p->addIdx(idx);
    return append(p);
  }

  IR::Value *var(IR::Type &t, std::string name) {
    return toValue(t, name);
  }

  IR::Value *val(IR::Type &t, int64_t val) {
    return toValue(t, val);
  }

  IR::Value *val(IR::Type &t, llvm::APInt val) {
    return toValue(t, val);
  }

  template<typename T>
  IR::Value *ret(IR::Type &t, T ret) {
    return append(
      std::make_unique<IR::Return>(t, *toValue(t, ret)));
  }

  template <typename T>
  void assume(T &&V) {
    auto AI =
      std::make_unique<IR::Assume>(*std::move(V), IR::Assume::Kind::IfNonPoison);
    F.getBB("").addInstr(std::move(AI));
  }

  template <typename A>
  IR::Value *unaryOp(IR::Type &t, std::string name, A a,
                     IR::UnaryOp::Op Op) {
    return append
      (std::make_unique<IR::UnaryOp>
      (t, std::move(name), *toValue(t, a), Op));
  }

  // Unimplemented : Freeze, CopyOp, Unreachable

private:
  template <typename T>
  IR::Value *append(T &&p) {
    auto ptr = p.get();
    F.getBB("").addInstr(std::move(p));
    return ptr;
  }

  IR::Function &F;
  std::unordered_map<std::string, IR::Value *> identifiers;

  IR::Value *toValue(IR::Type &t, int64_t x) {
    auto c = std::make_unique<IR::IntConst>(t, x);
    auto ptr = c.get();
    F.addConstant(std::move(c));
    return ptr;
  }

  IR::Value *toValue(IR::Type &t, llvm::APInt x) {
    auto c = std::make_unique<IR::IntConst>(t, x.toString(10, false));
    auto ptr = c.get();
    F.addConstant(std::move(c));
    return ptr;
  }

  IR::Value *toValue(IR::Type &t, std::string x) {
    if (auto It = identifiers.find(x); It != identifiers.end()) {
      return It->second;
    } else {
      if (x.find(souper::ReservedConstPrefix) != std::string::npos) {
        auto i = std::make_unique<IR::ConstantInput>(t, std::move(x));
        auto ptr = i.get();
//         F.addInput(std::move(i));
        F.addConstant(std::move(i));
        identifiers[x] = ptr;
        return ptr;
      }
      auto i = std::make_unique<IR::Input>(t, std::move(x));
      auto ptr = i.get();
      F.addInput(std::move(i));
      identifiers[x] = ptr;
      return ptr;
    }
  }

  IR::Value *toValue(IR::Type &t, const char *x) {
    return toValue(t, std::string(x));
  }

  IR::Value *toValue(IR::Type &, IR::Value *v) {
    return v;
  }
};



void getReservedConsts(souper::Inst *I,
                       std::map<std::string, souper::Inst *> &Result,
                       std::set<souper::Inst *> &Visited) {
  if (I->K == souper::Inst::Var && I->SynthesisConstID != 0) {
    Result["%" + souper::ReservedConstPrefix + std::to_string(I->SynthesisConstID)] = I;
  }
  for (auto &&Op : I->Ops) {
    if (Visited.find(Op) == Visited.end()) {
      getReservedConsts(Op, Result, Visited);
    }
  }
  Visited.insert(I);
}

struct ReturnLHSRAII {
  tools::Transform &t;
  IR::Function &LHS;
  ~ReturnLHSRAII() {
    LHS = std::move(t.src);
  }
};
}

std::map<souper::Inst *, llvm::APInt>
performCegisFirstQuery(tools::Transform &t,
                       std::map<std::string, souper::Inst *> &SouperConsts,
                       smt::expr &TriedExpr) {
  IR::State SrcState(t.src, true);
  IR::State TgtState(t.tgt, false);
  util::sym_exec(SrcState);
  util::sym_exec(TgtState);

  auto &&Sv = SrcState.returnVal();
  auto &&Tv = TgtState.returnVal();

  std::map<souper::Inst *, llvm::APInt> SynthesisResult;
  SynthesisResult.clear();

  std::set<smt::expr> Vars;
  std::map<std::string, smt::expr> SMTConsts;
  for (auto &[Var, Val, Pred] : TgtState.getValues()) {
    auto &Name = Var->getName();
    if (startsWith("%reservedconst", Name)) {
      SMTConsts[Name] = Val.first.value;
    }
  }

  if (SkipAliveSolver)
    return SynthesisResult;

  // TODO: implement synthesis with refinement
  smt::Solver::check({{(Sv.first.value == Tv.first.value) && (TriedExpr),
          [&](const smt::Result &R) {

          // no more guesses, stop immediately
          if (R.isUnsat()) {
            if (DebugLevel > 3)
              llvm::errs()<<"No more new possible guesses\n";
            return;
          } else if (R.isSat()) {
            auto &&Model = R.getModel();
            smt::expr TriedAnte(false);

            for (auto &[name, expr] : SMTConsts) {
              TriedAnte |= (expr != smt::expr::mkUInt(Model.getInt(expr), expr.bits()));
            }
            TriedExpr &= TriedAnte;

            for (auto &[name, expr] : SMTConsts) {
              auto *I = SouperConsts[name];
              SynthesisResult[I] = llvm::APInt(I->Width, Model.getInt(expr));
            }
          }
        }}});

  return SynthesisResult;
}

std::map<souper::Inst *, llvm::APInt>
synthesizeConstantUsingSolver(tools::Transform &t,
  std::map<std::string, souper::Inst *> &SouperConsts) {
  return {};

  IR::State SrcState(t.src, true), tgt_state(t.tgt, false);
  util::sym_exec(SrcState);
  util::sym_exec(tgt_state);

  util::Errors Errs;

  auto &&SrcRet = SrcState.returnVal();
  auto &&TgtRet = tgt_state.returnVal();

  auto QVars = SrcState.getQuantVars();
  QVars.insert(SrcRet.second.begin(), SrcRet.second.end());

  auto ErrF = [&](const smt::Result &r, bool print_var, const char *msg) {
//     tools::error(Errs, SrcState, tgt_state, r, print_var, nullptr,
//                  SrcRet.first, TgtRet.first, msg,false);
    //FIXME: temporarily disabled, find a way to pass a Type to tools::error
    std::cerr << msg << "\n";
    tools::TransformPrintOpts Opts;
    Opts.print_fn_header = true;
    t.print(std::cerr, Opts);
  };

  std::set<smt::expr> Vars;
  std::map<std::string, smt::expr> SMTConsts;

  for (auto &[var, val, Pred] : SrcState.getValues()) {
    auto &name = var->getName();
    if (startsWith("%var", name)) {
      auto app = val.first.value.isApp();
      assert(app);
      Vars.insert(Z3_get_app_arg(smt::ctx(), app, 1));
    }
  }
  for (auto &[var, val, Pred] : tgt_state.getValues()) {
    auto &name = var->getName();
    if (startsWith("%reserved", name)) {
      auto app = val.first.value.isApp();
      assert(app);
      SMTConsts[name] = (Z3_get_app_arg(smt::ctx(), app, 1));
    }
  }

  auto SimpleConstExistsCheck =
    smt::expr::mkForAll(Vars, SrcRet.first.value == TgtRet.first.value);

  std::map<souper::Inst *, llvm::APInt> SynthesisResult;

  if (SkipAliveSolver)
    return SynthesisResult;

  smt::Solver::check({{preprocess(t, QVars, SrcRet.second,
                       std::move(SimpleConstExistsCheck)),
  [&] (const smt::Result &R) {
    if (R.isUnsat()) {
      ErrF(R, true, "Value mismatch");
    } else if (R.isSat()) {
      auto &&Model = R.getModel();
      for (auto &[name, expr] : SMTConsts) {
        auto *I = SouperConsts[name];
        SynthesisResult[I] = llvm::APInt(I->Width, Model.getInt(expr));
      }
      return;
    } else {
      ErrF(R, true, "Unknown/Invalid Result, investigate.");
    }
  }}});

  return SynthesisResult;
}

souper::AliveDriver::AliveDriver(Inst *LHS_, Inst *PreCondition_, InstContext &IC_,
                                 std::vector<Inst *> ExtraInputs)
    : LHS(LHS_), PreCondition(PreCondition_), IC(IC_) {
  IsLHS = true;
  InstNumbers = 101;
  //FIXME: Magic number. 101 is chosen arbitrarily.
  //This should go away once non-input variable names are not discarded

  // If alive ever supports src and tgt with different number of arguments in future, this can go away.
  for (auto Extra : ExtraInputs) {
    if (!translateAndCache(Extra, LHSF, LExprCache)) {
      llvm::report_fatal_error("Warning: Failed to translate extra input.\n");
    }
  }

  if (!translateRoot(LHS, PreCondition, LHSF, LExprCache)) {
    ReplacementContext RC;
    RC.printInst(LHS, llvm::outs(), true);
    llvm::report_fatal_error("Failed to translate LHS.\n");
  }

  if (DisableUndefInput) {
    util::config::disable_undef_input = true;
  }
  IsLHS = false;
}

//TODO: Return an APInt when alive supports it
std::map<souper::Inst *, llvm::APInt>
souper::AliveDriver::synthesizeConstants(souper::Inst *RHS) {
  std::map<Inst *, llvm::APInt> Result;
  InstNumbers = 0;
  std::map<std::string, Inst *> Consts;
  std::set<Inst *> Visited;
  getReservedConsts(RHS, Consts, Visited);
  ReplacementContext RC;
  RC.printInst(RHS, llvm::errs(), true);
  assert(!Consts.empty());
  RExprCache.clear();
  IR::Function RHSF;
  if (!translateRoot(RHS, nullptr, RHSF, RExprCache)) {
    if (DebugLevel > 2)
      llvm::errs() << "Failed to translate RHS.\n";
    // TODO: Eventually turn this into an assertion
    return {};
  }
  tools::Transform t;
  ReturnLHSRAII foo{t, LHSF};
  t.src = std::move(LHSF);
  t.tgt = std::move(RHSF);
  tools::TransformVerify tv(t, /*check_each_var=*/false);

  return synthesizeConstantUsingSolver(t, Consts);
}

std::map<souper::Inst *, llvm::APInt>
souper::AliveDriver::synthesizeConstantsWithCegis(souper::Inst *RHS, InstContext &IC) {
  std::map<souper::Inst *, llvm::APInt> ConstMap;
  std::map<std::string, Inst *> Consts;
  std::set<Inst *> Visited;
  getReservedConsts(RHS, Consts, Visited);
  assert(!Consts.empty());

  smt::expr TriedExpr(true);

  RExprCache.clear();
  IR::Function RHSF;
  if (!translateRoot(RHS, PreCondition, RHSF, RExprCache)) {
    if (DebugLevel > 2)
      llvm::errs() << "Failed to translate RHS.\n";
    // TODO: Eventually turn this into an assertion
    return {};
  }

  tools::Transform t;
  t.tgt = std::move(RHSF);

  unsigned Tried = 0;
  while (true) {
    // First Query
    {
      ReturnLHSRAII foo{t, LHSF};
      t.src = std::move(LHSF);

      Tried ++;
      // exceeds MAX_TRIES, stop immediately
      if (Tried > MaxTries) {
        if (DebugLevel > 2)
          llvm::errs() << "Time of tries reached maximum\n";
        return {};
      }

      ConstMap = performCegisFirstQuery(t, Consts, TriedExpr);

      // stop immediately if first query is unsat.
      if (ConstMap.empty())
        return {};
    }

    // plug constants into guess
    std::map<Inst *, Inst *> InstCache;
    std::map<Block *, Block *> BlockCache;
    auto GWithC = getInstCopy(RHS, IC, InstCache, BlockCache, &ConstMap,
                              /*CloneVars=*/false);

    // Second Query
    {
      if (verify(GWithC, PreCondition)) {
        break;
      } else {
        continue;
      }
    }
  }
  return ConstMap;
}

void souper::AliveDriver::copyInputs(souper::AliveDriver::Cache &To,
                                     IR::Function &RHS) {
  for (auto &[I, Val] : Inputs) {
    if (I->K == Inst::Kind::Var) {
      auto Input = std::make_unique<IR::Input>(Val->getType(),
                                               std::string(NameMap[I]));
      To[I] = Input.get();
      RHS.addInput(std::move(Input));
    }
  }
}

bool souper::AliveDriver::verify (Inst *RHS, Inst *RHSAssumptions) {
  RExprCache.clear();
  IR::Function RHSF;
  copyInputs(RExprCache, RHSF);
  if (!translateRoot(RHS, RHSAssumptions, RHSF, RExprCache)) {
    llvm::errs() << "Failed to translate RHS.\n";
    // TODO: Eventually turn this into an assertion
    return false;
  }

  if (DebugLevel > 2) {
    std::cerr << "Verifying following Alive Transformation ... \n\n";
    std::cerr << LHSF << '\n';
    std::cerr << " => \n\n";
    std::cerr << RHSF << '\n';
  }

  tools::Transform t;
  ReturnLHSRAII foo{t, LHSF};
  t.src = std::move(LHSF);
  t.tgt = std::move(RHSF);
  tools::TransformVerify tv(t, /*check_each_var=*/false);

  if (SkipAliveSolver)
    return false;

  if (auto errs = tv.verify()) {
    if (DebugLevel >= 1) {
      std::ostringstream os;
      os << errs << "\n";
      llvm::errs() << os.str();
    }
    return false; // TODO: Encode errs into ErrorCode
  } else {
    if (DebugLevel > 2)
      llvm::errs() << "RHS proved valid.\n";
    return true;
  }
}

bool souper::AliveDriver::translateRoot(const souper::Inst *I, const Inst *PC,
                                        IR::Function &F, Cache &ExprCache) {
  if (!translateAndCache(I, F, ExprCache)) {
    return false;
  }
  if (PC && !translateAndCache(PC, F, ExprCache)) {
    return false;
  }

  translateDemandedBits(I, F, ExprCache);

  FunctionBuilder Builder(F);
  if (PC) {
    auto Zero = Builder.val(getType(I->Width), llvm::APInt(I->Width, 0));
    ExprCache[I] = Builder.select(getType(I->Width), "%ifpc",
                   ExprCache[PC], ExprCache[I], Zero);
  }
  Builder.ret(getType(I->Width), ExprCache[I]);
  F.setType(getType(I->Width));
  return true;
}

// Dummy because it doesn't actually build expressions.
// It exists for the purpose of reusing parts of the abstract ExprBuilder here.
// FIXME: Allow creating objects of ExprBuilder
class DummyExprBuilder : public souper::ExprBuilder {
public:
  DummyExprBuilder(souper::InstContext &IC) : souper::ExprBuilder(IC) {}
  std::string BuildQuery(const souper::BlockPCs & BPCs,
                         const std::vector<souper::InstMapping> & PCs,
                         souper::InstMapping Mapping,
                         std::vector<souper::Inst *> * ModelVars,
                         souper::Inst *Precondition,
                         bool Negate, bool DropUB) override {
    llvm::report_fatal_error("Do not call");
    return "";
  }
  std::string GetExprStr(const souper::BlockPCs & BPCs,
                         const std::vector<souper::InstMapping> & PCs,
                         souper::InstMapping Mapping,
                         std::vector<souper::Inst *> * ModelVars,
                         bool Negate, bool DropUB) override {
    llvm::report_fatal_error("Do not call");
    return "";
  }
};
std::string getUniqueName() {
  static int N = 0;
  return "dummy_" + std::to_string(N++);
}

bool souper::AliveDriver::translateAndCache(const souper::Inst *I,
                                            IR::Function &F,
                                            Cache &ExprCache) {
  // unused translation; this is souper's internal instruction to represent overflow instructions
  if (souper::Inst::isOverflowIntrinsicSub(I->K)) {
    return true;
  }

  if (ExprCache.find(I) != ExprCache.end()) {
    return true; // Already translated
  }

  auto Ops = I->Ops;
  if (souper::Inst::isOverflowIntrinsicMain(I->K)) {
    Ops = Ops[0]->Ops;
  }

  FunctionBuilder Builder(F);
  for (auto &&Op : Ops) {
    if (!translateAndCache(Op, F, ExprCache)) {
      return false;
    }
    if (I->K == Inst::ExtractValue) {
      break; // Break after translating main arg, idx is handled separately.
    }
  }

  std::string Name = "";

  if (NamesCache.find(I) != NamesCache.end()) {
    Name = NamesCache[I];
  } else if (I->Name != "") {
    if (I->SynthesisConstID != 0) {
      // No way to avoid string matching without
      // changes in Inst and EnumerativeSynthesis
      Name = "%" + souper::ReservedConstPrefix + std::to_string(I->SynthesisConstID);
    } else {
      Name = "%var_" + I->Name;
    }
  } else {
    Name = "%" + std::to_string(InstNumbers++);
    // FIXME: Somewhere the non-input variable names are discarded,
    // forcing AliveDriver to name variables on its own.
  }
  if (I->K == Inst::Var) {
    NameMap[I] = Name;
  }

  auto &t = getType(I->Width);

  switch (I->K) {
    case souper::Inst::Var: {
      ExprCache[I] = Builder.var(t, Name);
      if (IsLHS) {
        Inputs.push_back({I, ExprCache[I]});
      }
      return translateDataflowFacts(I, F, ExprCache);
    }
    case souper::Inst::Hole: {
//       ExprCache[I] = Builder.freeze(t, getUniqueName(),Builder.var(t, getUniqueName()));
      ExprCache[I] = Builder.var(t, getUniqueName());
      return true;
    }
    case souper::Inst::Const: {
      ExprCache[I] = Builder.val(t, I->Val);
      return true;
    }

    case souper::Inst::Select: {
      ExprCache[I] = Builder.select(t, Name,
        ExprCache[I->Ops[0]],
        ExprCache[I->Ops[1]],
        ExprCache[I->Ops[2]]);
      return true;
    }

    case souper::Inst::Phi: {
      if (I->Ops.size() != 1) {
        assert(false && "Phi with muliple arguments unimplemented");
        return false;
      }
      ExprCache[I] = ExprCache[I->Ops[0]];
      return true;
    }

    case souper::Inst::ExtractValue: {
      unsigned idx = I->Ops[1]->Val.getLimitedValue();
      assert(idx <= 1 && "Only extractvalue with overflow instructions are supported.");
      if (idx == 0) {
        t = getType(I->Ops[0]->Width - 1);
      } else {
        t = getType(1);
      }
      ExprCache[I] = Builder.extractvalue(t, Name, ExprCache[I->Ops[0]], idx);
      return true;
    }

    case souper::Inst::Freeze: {
      ExprCache[I] = Builder.freeze(t, Name, ExprCache[I->Ops[0]]);
      return true;
    }

    #define BINOP(SOUPER, ALIVE) case souper::Inst::SOUPER: {    \
      ExprCache[I] = Builder.binOp(t, Name, ExprCache[I->Ops[0]],\
      ExprCache[I->Ops[1]], IR::BinOp::ALIVE);                   \
      return true;                                               \
    }

    #define BINOPOV(SOUPER, ALIVE) case souper::Inst::SOUPER: {  \
      ExprCache[I] = Builder.binOp(getOverflowType               \
      (I->Ops[0]->Width), Name, ExprCache[Ops[0]],               \
      ExprCache[Ops[1]], IR::BinOp::ALIVE);                      \
      return true;                                               \
    }

    #define BINOPF(SOUPER, ALIVE, W) case souper::Inst::SOUPER: {\
      ExprCache[I] = Builder.binOp(t, Name, ExprCache[I->Ops[0]],\
      ExprCache[I->Ops[1]], IR::BinOp::ALIVE, IR::BinOp::W);     \
      return true;                                               \
    }

    #define FAKEBINOP(SOUPER, ALIVE) case souper::Inst::SOUPER: {\
      ExprCache[I] = Builder.binOp(t, Name, ExprCache[I->Ops[0]],\
      Builder.val(getType(1), 0), IR::BinOp::ALIVE);             \
      return true;                                               \
    }

    BINOP(Add, Add);
    BINOPF(AddNSW, Add, NSW);
    BINOPF(AddNUW, Add, NUW);
    BINOPF(AddNW, Add, NSW | IR::BinOp::NUW); //FIXME(manasij) : Leaky abstraction
    BINOP(Sub, Sub);
    BINOPF(SubNSW, Sub, NSW);
    BINOPF(SubNUW, Sub, NUW);
    BINOPF(SubNW, Sub, NSW | IR::BinOp::NUW);
    BINOP(Mul, Mul);
    BINOPF(MulNSW, Mul, NSW);
    BINOPF(MulNUW, Mul, NUW);
    BINOPF(MulNW, Mul, NSW | IR::BinOp::NUW);
    BINOP(And, And);
    BINOP(Or, Or);
    BINOP(Xor, Xor);
    BINOP(Shl, Shl);
    BINOPF(ShlNSW, Shl, NSW);
    BINOPF(ShlNUW, Shl, NUW);
    BINOPF(ShlNW, Shl, NSW | IR::BinOp::NUW);
    BINOP(LShr, LShr);
    BINOPF(LShrExact, LShr, Exact);
    BINOP(AShr, AShr);
    BINOPF(AShrExact, AShr, Exact);
    FAKEBINOP(Cttz, Cttz);
    FAKEBINOP(Ctlz, Ctlz);
    BINOP(URem, URem);
    BINOP(SRem, SRem);
    BINOP(UDiv, UDiv);
    BINOPF(UDivExact, UDiv, Exact);
    BINOP(SDiv, SDiv);
    BINOPF(SDivExact, SDiv, Exact);
    BINOP(SAddSat, SAdd_Sat)
    BINOP(UAddSat, UAdd_Sat)
    BINOP(SSubSat, SSub_Sat)
    BINOP(USubSat, USub_Sat)
    BINOPOV(SAddWithOverflow, SAdd_Overflow)
    BINOPOV(UAddWithOverflow, UAdd_Overflow)
    BINOPOV(SSubWithOverflow, SSub_Overflow)
    BINOPOV(USubWithOverflow, USub_Overflow)
    BINOPOV(SMulWithOverflow, SMul_Overflow)
    BINOPOV(UMulWithOverflow, UMul_Overflow)

    #define TERNOP(SOUPER, ALIVE) case souper::Inst::SOUPER: {   \
      ExprCache[I] = Builder.ternaryOp(t, Name, ExprCache        \
      [I->Ops[0]], ExprCache[I->Ops[1]], ExprCache[I->Ops[2]],   \
      IR::TernaryOp::ALIVE); return true;                        \
    }

    TERNOP(FShl, FShl);
    TERNOP(FShr, FShr);

    #define ICMP(SOUPER, ALIVE) case souper::Inst::SOUPER: {     \
      ExprCache[I] = Builder.iCmp(t, Name, IR::ICmp::ALIVE,      \
      ExprCache[I->Ops[0]], ExprCache[I->Ops[1]]);               \
      return true;                                               \
    }

    ICMP(Eq, EQ);
    ICMP(Ne, NE);
    ICMP(Ule, ULE);
    ICMP(Ult, ULT);
    ICMP(Sle, SLE);
    ICMP(Slt, SLT);

    #define CONVOP(SOUPER, ALIVE) case souper::Inst::SOUPER: {   \
      ExprCache[I] = Builder.conversionOp(t, Name,               \
      ExprCache[I->Ops[0]], IR::ConversionOp::ALIVE);            \
      return true;                                               \
    }

    CONVOP(SExt, SExt);
    CONVOP(ZExt, ZExt);
    CONVOP(Trunc, Trunc);

    #define UNARYOP(SOUPER, ALIVE) case souper::Inst::SOUPER: {  \
      ExprCache[I] = Builder.unaryOp(t, Name,                    \
      ExprCache[I->Ops[0]], IR::UnaryOp::ALIVE);                 \
      return true;                                               \
    }

    UNARYOP(CtPop, Ctpop);
    UNARYOP(BSwap, BSwap);
    UNARYOP(BitReverse, BitReverse);

    default:{
      llvm::outs() << "Unsupported Instruction Kind : " << I->getKindName(I->K) << "\n";
      return false;
    }
  }
}
bool
souper::AliveDriver::translateDataflowFacts(const souper::Inst* I,
                                            IR::Function& F,
                                            souper::AliveDriver::Cache& ExprCache) {
  DummyExprBuilder EB(IC);
  auto DataFlowConstraints = EB.getDataflowConditions(const_cast<Inst *>(I));
  //FIXME: Get rid of the const_cast by making getDataflowConditions take const Inst *
  if (DataFlowConstraints) {
    if (!translateAndCache(DataFlowConstraints, F, ExprCache)) {
      return false;
    }
    FunctionBuilder Builder(F);
    Builder.assume(ExprCache[DataFlowConstraints]);
    return true;
  } else {
    return false;
  }
}

void
souper::AliveDriver::translateDemandedBits(const souper::Inst* I,
                     IR::Function& F,
                     souper::AliveDriver::Cache& ExprCache) {
  FunctionBuilder Builder(F);

  auto DemandedBits = IsLHS ? I->DemandedBits : LHS->DemandedBits;

  assert(DemandedBits.getBitWidth() == I-> Width && "Uninitialized DemandedBits");

  if (!DemandedBits.isAllOnesValue()) {
    auto DBMask = Builder.val(getType(I->Width), DemandedBits);

    ExprCache[I] = Builder.binOp(getType(I->Width),
                                 "%" + std::to_string(InstNumbers++), ExprCache[I],
                                 DBMask, IR::BinOp::And);
  }
}

IR::Type &souper::AliveDriver::getType(int Width) {
  std::string n = "i" + std::to_string(Width);
  if (TypeCache.find(n) == TypeCache.end()) {
    TypeCache[n] = new IR::IntType(std::move(n), Width);
  }
  return *TypeCache[n];
}

IR::Type &souper::AliveDriver::getOverflowType(int Width) {
  std::string n = "o" + std::to_string(Width);
  if (TypeCache.find(n) == TypeCache.end()) {
    std::vector<IR::Type *> Types = {&getType(Width), &getType(1)};
    std::vector<bool> Padding = {false, false};
    TypeCache[n] = new IR::StructType(std::move(n),std::move(Types),
                                      std::move(Padding));
  }
  return *TypeCache[n];
}

namespace souper {
void collectPhis(souper::Inst *I, std::map<souper::Block *, std::set<souper::Inst *>> &Phis) {
  std::vector<Inst *> Stack{I};
  std::unordered_set<Inst *> Visited;
  while (!Stack.empty()) {
    auto Current = Stack.back();
    Stack.pop_back();
    if (Current->K == Inst::Phi) {
      Phis[Current->B].insert(Current);
    }
    Visited.insert(Current);
    for (auto Child : Current->Ops) {
      if (Visited.find(Child) == Visited.end()) {
        Stack.push_back(Child);
      }
    }
  }
}

struct RefinementProblem {
  souper::Inst *LHS;
  souper::Inst *RHS;
  souper::Inst *Pre;
  BlockPCs BPCs;

  RefinementProblem ReplacePhi(souper::InstContext &IC, std::map<Block *, size_t> &Change) {
    std::map<souper::Block *, std::set<souper::Inst *>> Phis;
    collectPhis(LHS, Phis);
    collectPhis(Pre, Phis);
    for (auto &BPC : BPCs) {
      collectPhis(BPC.PC.LHS, Phis);
    }

    if (Phis.empty()) {
      return *this; // Base case, no more Phis
    }

    std::map<Inst *, Inst *> InstCache;
    for (auto Pair : Phis) {
      for (auto Phi : Pair.second) {
        InstCache[Phi] = Phi->Ops[Change[Pair.first]];
      }
    }
    std::map<Block *, Block *> BlockCache;
    std::map<Inst *, llvm::APInt> ConstMap;
    RefinementProblem Result;
    Result.LHS = getInstCopy(LHS, IC, InstCache, BlockCache, &ConstMap, false);
    Result.RHS = getInstCopy(RHS, IC, InstCache, BlockCache, &ConstMap, false);
    Result.Pre = getInstCopy(Pre, IC, InstCache, BlockCache, &ConstMap, false);
    Result.BPCs = BPCs;
    for (auto &BPC : Result.BPCs) {
      BPC.PC.LHS = getInstCopy(BPC.PC.LHS, IC, InstCache, BlockCache,
                               &ConstMap, false);
    }

    // Recursively call ReplacePhi, because Result might have Phi`s
    return Result.ReplacePhi(IC, Change);
  }
  bool operator == (const RefinementProblem &P) const {
    if (LHS == P.LHS && RHS == P.RHS &&
        Pre == P.Pre && BPCs.size() == P.BPCs.size()) {
      for (size_t i = 0; i < BPCs.size(); ++i) {
        if (BPCs[i].B != P.BPCs[i].B ||
            BPCs[i].PC.LHS != P.BPCs[i].PC.LHS ||
            BPCs[i].PC.RHS != P.BPCs[i].PC.RHS) {
          return false;
        }
      }
      return true;
    } else {
      return false;
    }
  }
  struct Hash
  {
    std::size_t operator()(const RefinementProblem &P) const
    {
      return std::hash<Inst *>()(P.LHS)
             ^ std::hash<Inst *>()(P.RHS) << 1
             ^ std::hash<Inst *>()(P.Pre) << 2
             ^ std::hash<size_t>()(P.BPCs.size());
    }
  };

};

std::unordered_set<RefinementProblem, RefinementProblem::Hash>
  explodePhis(InstContext &IC, RefinementProblem P) {
  std::map<souper::Block *, std::set<souper::Inst *>> Phis;
  collectPhis(P.LHS, Phis);
  collectPhis(P.Pre, Phis);

  if (Phis.empty()) {
    return {P};
  }

  std::vector<Block *> Blocks;
  for (auto &&Pair : Phis) {
    Blocks.push_back(Pair.first);
  }

  std::vector<std::map<Block *, size_t>> ChangeList;

  for (size_t i = 0; i < Blocks.size(); ++i) { // Each block
    if (i == 0) {
      for (size_t j = 0; j < Blocks[i]->Preds; ++j) {
        ChangeList.push_back({{Blocks[i], j}});
      }
    } else {
      std::vector<std::map<Block *, size_t>> NewChangeList;
      for (size_t j = 0; j < Blocks[i]->Preds; ++j) {
        for (auto Change : ChangeList) {
          Change.insert({Blocks[i], j});
          NewChangeList.push_back(Change);
        }
      }
      std::swap(ChangeList, NewChangeList);
    }
  }

  std::unordered_set<RefinementProblem, RefinementProblem::Hash> Result;

  for (auto Change : ChangeList) {
    auto Goal = P.ReplacePhi(IC, Change);
    // Consider switching to better data structures for dealing with BPCs
    for (auto &[Block, Pred] : Change) {
      for (auto &BPC : Goal.BPCs) {
        if (BPC.B == Block && BPC.PredIdx == Pred) {
          auto Ante = IC.getInst(Inst::Eq, 1, {BPC.PC.LHS, BPC.PC.RHS});
          Goal.Pre = IC.getInst(Inst::And, 1, {Goal.Pre, Ante});
        }
      }
    }
    Result.insert(Goal);
  }
  return Result;
}

}
bool souper::isTransformationValid(souper::Inst *LHS, souper::Inst *RHS,
                                   const std::vector<InstMapping> &PCs,
                                   const souper::BlockPCs &BPCs,
                                   InstContext &IC) {
  Inst *Ante = IC.getConst(llvm::APInt(1, true));
  for (auto PC : PCs ) {
    Inst *Eq = IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
    Ante = IC.getInst(Inst::And, 1, {Ante, Eq});
  }

  auto Goals = explodePhis(IC, {LHS, RHS, Ante, BPCs});
  // Alive2 and Souper have different PHI semantics.
  // The current solution decomposes problems with PHI nodes into
  // simpler problems without PHI.
  // Example: f(x, y) = phi %block, %x, %y is decomposed into two functions:
  // * f_1(x, y) = %x
  // * f_2(x, y) = %y

  if (SkipAliveSolver)
    return false;
  if (DebugLevel > 3)
    llvm::errs() << "Number of sub-goals : " << Goals.size() << "\n";
  for (auto Goal : Goals) {
    if (DebugLevel > 3) {
      llvm::errs() << "Goal:\n";
      ReplacementContext RC;
      RC.printInst(Goal.LHS, llvm::errs(), true);
      llvm::errs() << "\n------\n";
    }
    std::vector<Inst *> Vars;
    findVars(Goal.RHS, Vars);
    AliveDriver Verifier(Goal.LHS, Goal.Pre, IC, Vars);
    if (!Verifier.verify(Goal.RHS, Goal.Pre))
      return false;
  }
  return true;
}


bool souper::isCandidateInfeasible(souper::Inst* RHS, souper::ValueCache& C,
                                   llvm::APInt LHSValue, InstContext &IC) {

  auto LHS = IC.getConst(LHSValue);
  // TODO: Use PC
  AliveDriver Pruner(LHS, nullptr, IC);

  Inst *RHSAssume = IC.getConst(llvm::APInt(1, true));
  for (auto P : C) {
    if (P.second.hasValue()) {
      auto *Eq = IC.getInst(Inst::Eq, P.first->Width,
                            {P.first, IC.getConst(P.second.getValue())});
      RHSAssume = IC.getInst(Inst::And, 1, {RHSAssume, Eq});
      ReplacementContext RC;
      RC.printInst(RHSAssume, llvm::errs(), true);
    } else {
      return false;
    }
  }

  return !Pruner.verify(RHS, RHSAssume);
}
