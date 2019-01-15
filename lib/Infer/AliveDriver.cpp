#include "souper/Infer/AliveDriver.h"

#include "alive2/ir/function.h"
#include "alive2/ir/instr.h"
#include "alive2/ir/state.h"
#include "alive2/smt/ctx.h"
#include "alive2/smt/expr.h"
#include "alive2/smt/smt.h"
#include "alive2/smt/solver.h"
#include "alive2/tools/transform.h"
#include "alive2/util/errors.h"
#include "alive2/util/symexec.h"

#include "llvm/Support/raw_ostream.h"

#include <memory>
#include <set>
#include <string_view>
#include <z3.h>

namespace {
class FunctionBuilder {
public:
  FunctionBuilder(IR::Function &F_) : F(F_) {}

  template <typename A, typename B, typename ...Others>
  IR::Value *binOp(IR::Type &t, std::string name, A a, B b,Others... others) {
    return append
      (std::make_unique<IR::BinOp>
      (t, std::move(name), *toValue(t, a), *toValue(t, b), others...));
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

  IR::Value *var(IR::Type &t, std::string name) {
    return toValue(t, name);
  }

  IR::Value *val(IR::Type &t, int64_t val) {
    return toValue(t, val);
  }

  template<typename T>
  IR::Value *ret(IR::Type &t, T ret) {
    return append(
      std::make_unique<IR::Return>(t, *toValue(t, ret)));
  }

  template <typename T>
  void assume(T &&V) {
    auto AI = std::make_unique<IR::Assume>(*std::move(V));
    F.getBB("").addIntr(std::move(AI));
  }

  // Unimplemented : Freeze, CopyOp, Unreachable

private:
  template <typename T>
  IR::Value *append(T &&p) {
    auto ptr = p.get();
    F.getBB("").addIntr(std::move(p));
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

  IR::Value *toValue(IR::Type &t, std::string x) {
    if (auto It = identifiers.find(x); It != identifiers.end()) {
      return It->second;
    } else {
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


bool startsWith(const std::string &pre, const std::string &str) {
  return std::equal(pre.begin(), pre.end(), str.begin());
}

void getReservedConsts(souper::Inst *I, std::set<std::string> &Result) {
  if (startsWith("reservedconst_", I->Name)) {
    Result.insert(I->Name);
  }
  for (auto &&Op : I->Ops) {
    getReservedConsts(Op, Result);
  }
}

struct ReturnLHSRAII {
  tools::Transform &t;
  IR::Function &LHS;
  ~ReturnLHSRAII() {
    LHS = std::move(t.src);
  }
};
}

util::Errors synthesizeConstantUsingSolver(tools::Transform &t,
                                           int64_t &result) {

  IR::Value::reset_gbl_id();
  IR::State SrcState(t.src), tgt_state(t.tgt);
  util::sym_exec(SrcState);
  util::sym_exec(tgt_state);

  util::Errors Errs;

  auto &SrcRet = SrcState.returnVal();
  auto &TgtRet = tgt_state.returnVal();

  auto QVars = SrcState.getQuantVars();
  QVars.insert(SrcRet.second.begin(), SrcRet.second.end());

  auto ErrF = [&](const smt::Result &r, bool print_var, const char *msg) {
    tools::error(Errs, SrcState, tgt_state, r, print_var, nullptr,
                 SrcRet.first, TgtRet.first, msg,false);
  };

  std::set<smt::expr> Vars;
  std::set<smt::expr> Consts;

  for (auto &[var, val] : SrcState.getValues()) {
    auto &name = var->getName();
    if (startsWith("%var", name)) {
      auto app = val.first.value.isApp();
      assert(app);
      Vars.insert(Z3_get_app_arg(smt::ctx(), app, 1));
    }
  }
  for (auto &[var, val] : tgt_state.getValues()) {
    auto &name = var->getName();
    if (startsWith("%reserved", name)) {
      auto app = val.first.value.isApp();
      assert(app);
      Consts.insert(Z3_get_app_arg(smt::ctx(), app, 1));
    }
  }

  auto SimpleConstExistsCheck =
    smt::expr::mkForAll(Vars, SrcRet.first.value == TgtRet.first.value);

  smt::Solver::check({{preprocess(t, QVars, SrcRet.second,
                       std::move(SimpleConstExistsCheck)),
  [&] (const smt::Result &R) {
    if (R.isUnsat()) {
      ErrF(R, true, "Value mismatch");
    } else if (R.isSat()) {
      result = R.getModel().getInt(*Consts.begin());
      return;
    } else {
      ErrF(R, true, "Unknown/Invalid Result, investigate.");
    }
  }}});

  return Errs;
}

souper::AliveDriver::AliveDriver(Inst *LHS_, Inst *PreCondition_)
    : LHS(LHS_), PreCondition(PreCondition_) {
  InstNumbers = 101;
  //FIXME: Magic number. 101 is chosen arbitrarily.
  //This should go away once non-input variable names are not discarded

  if (!translateRoot(LHS, PreCondition, LHSF, LExprCache)) {
    llvm::report_fatal_error("Failed to translate LHS.\n");
  }
}

//TODO: Return an APInt when alive supports it
std::optional<int64_t>
souper::AliveDriver::synthesizeConstant(souper::Inst *RHS) {
  InstNumbers = 0;
  std::set<std::string> Consts;
  getReservedConsts(RHS, Consts);
  assert(!Consts.empty());
  if (Consts.size() == 1) {
    RExprCache.clear();
    IR::Function RHSF;
    if (!translateRoot(RHS, nullptr, RHSF, RExprCache)) {
      llvm::errs() << "Failed to translate RHS.\n";
      // TODO: Eventually turn this into an assertion
      return std::nullopt;
    }
    tools::Transform t;
    ReturnLHSRAII foo{t, LHSF};
    t.src = std::move(LHSF);
    t.tgt = std::move(RHSF);
    tools::TransformVerify tv(t, /*check_each_var=*/false);
    int64_t result = 0;
    if (auto errs = synthesizeConstantUsingSolver(t, result)) {
      return std::nullopt; // TODO: Encode errs into ErrorCode
    } else {
      return {result};
    }
  } else {
    if (!Consts.empty())
      llvm::errs() << "Multiple constants unimplemented.\n";
    return std::nullopt;
  }
}


bool souper::AliveDriver::verify (Inst *RHS) {
  RExprCache.clear();
  IR::Function RHSF;
  if (!translateRoot(RHS, nullptr, RHSF, RExprCache)) {
    llvm::errs() << "Failed to translate RHS.\n";
    // TODO: Eventually turn this into an assertion
    return false;
  }

  tools::Transform t;
  ReturnLHSRAII foo{t, LHSF};
  t.src = std::move(LHSF);
  t.tgt = std::move(RHSF);
  tools::TransformVerify tv(t, /*check_each_var=*/false);

  if (auto errs = tv.verify()) {
    return false; // TODO: Encode errs into ErrorCode
  } else {
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
  FunctionBuilder Builder(F);
  if (PC) {
    Builder.assume(ExprCache[PC]);
  }
  Builder.ret(getType(I->Width), ExprCache[I]);
  F.setType(getType(I->Width));
  return true;
}


bool souper::AliveDriver::translateAndCache(const souper::Inst *I,
                                            IR::Function &F,
                                            Cache &ExprCache) {
  if (ExprCache.find(I) != ExprCache.end()) {
    return true; // Already translated
  }

  FunctionBuilder Builder(F);
  for (auto &&Op : I->Ops) {
    if (!translateAndCache(Op, F, ExprCache)) {
      return false;
    }
  }

  std::string Name = "";

  if (NamesCache.find(I) != NamesCache.end()) {
    Name = NamesCache[I];
  } else if (I->Name != "") {
    if (startsWith("reservedconst_", I->Name)) {
      // No way to avoid string matching without
      // changes in Inst and ExhaustiveSynthesis
      Name = "%" + I->Name;
    } else {
      Name = "%var_" + I->Name;
    }
  } else {
    Name = "%" + std::to_string(InstNumbers++);
    // FIXME: Somewhere the non-input variable names are discarded,
    // forcing AliveDriver to name variables on its own.
  }

  auto &t = getType(I->Width);

  switch (I->K) {
    case souper::Inst::Var: {
      ExprCache[I] = Builder.var(t, Name);
      return true;
    }
    case souper::Inst::Const: {
      ExprCache[I] = Builder.val(t, I->Val.getLimitedValue());
      return true;
    }

    case souper::Inst::Select: {
      ExprCache[I] = Builder.select(t, Name,
        ExprCache[I->Ops[0]],
        ExprCache[I->Ops[1]],
        ExprCache[I->Ops[2]]);
    }

    #define BINOP(SOUPER, ALIVE) case souper::Inst::SOUPER: {    \
      ExprCache[I] = Builder.binOp(t, Name, ExprCache[I->Ops[0]],\
      ExprCache[I->Ops[1]], IR::BinOp::ALIVE);                   \
      return true;                                               \
    }

    #define BINOPW(SOUPER, ALIVE, W) case souper::Inst::SOUPER: {\
      ExprCache[I] = Builder.binOp(t, Name, ExprCache[I->Ops[0]],\
      ExprCache[I->Ops[1]], IR::BinOp::ALIVE, IR::BinOp::W);     \
      return true;                                               \
    }

    BINOP(Add, Add);
    BINOPW(AddNSW, Add, NSW);
    BINOPW(AddNUW, Add, NUW);
    BINOP(Sub, Sub);
    BINOPW(SubNSW, Sub, NSW);
    BINOPW(SubNUW, Sub, NUW);
    BINOP(Mul, Mul);
    BINOP(And, And);
    BINOP(Or, Or);
    BINOP(Xor, Xor);
    BINOP(Shl, Shl);
    BINOPW(ShlNSW, Shl, NSW);
    BINOPW(ShlNUW, Shl, NUW);
    BINOP(LShr, LShr);
    BINOP(AShr, AShr);

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

    default:{
      llvm::outs() << "Unsupported Instruction Kind : " << I->getKindName(I->K) << "\n";
      return false;
    }
  }
}

IR::Type &souper::AliveDriver::getType(int n) {
  if (TypeCache.find(n) == TypeCache.end()) {
    TypeCache[n] = new IR::IntType("i" + std::to_string(n), n);
  }
  return *TypeCache[n];
}
