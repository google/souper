#include "souper/Infer/AliveDriver.h"
#include "souper/Extractor/ExprBuilder.h"

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
#include <sstream>
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
    auto AI =
      std::make_unique<IR::Assume>(*std::move(V), /*if_non_poison=*/ true);
      //TODO: FIgure out whether if_non_poison can be uconditionally true
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

void getReservedConsts(souper::Inst *I,
                       std::map<std::string, souper::Inst *> &Result,
                       std::set<souper::Inst *> &Visited) {
  if (startsWith("reservedconst_", I->Name)) {
    Result["%" + I->Name] = I;
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
synthesizeConstantUsingSolver(tools::Transform &t,
  std::map<std::string, souper::Inst *> &SouperConsts) {

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
  std::map<std::string, smt::expr> SMTConsts;

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
      SMTConsts[name] = (Z3_get_app_arg(smt::ctx(), app, 1));
    }
  }

  auto SimpleConstExistsCheck =
    smt::expr::mkForAll(Vars, SrcRet.first.value == TgtRet.first.value);

  std::map<souper::Inst *, llvm::APInt> SynthesisResult;

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

souper::AliveDriver::AliveDriver(Inst *LHS_, const BlockPCs &BPCs,
                                 const std::vector<InstMapping> &PCs,
                                 InstContext &IC_)
    : LHS(LHS_), IC(IC_), BPCs(BPCs), PCs(PCs) {
  InstNumbers = 101;
  //FIXME: Magic number. 101 is chosen arbitrarily.
  //This should go away once non-input variable names are not discarded

  PreCondition = IC.getConst(llvm::APInt(1, true));
  for (auto PC : PCs ) {
    Inst *Eq = IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
    PreCondition = IC.getInst(Inst::And, 1, {PreCondition, Eq});
  }

  if (!translateRoot(LHS, PreCondition, LHSF, LExprCache)) {
    llvm::report_fatal_error("Failed to translate LHS.\n");
  }
}

//TODO: Return an APInt when alive supports it
std::map<souper::Inst *, llvm::APInt>
souper::AliveDriver::synthesizeConstants(souper::Inst *RHS) {
  std::map<Inst *, llvm::APInt> Result;
  InstNumbers = 0;
  std::map<std::string, Inst *> Consts;
  std::set<Inst *> Visited;
  getReservedConsts(RHS, Consts, Visited);
  assert(!Consts.empty());
  RExprCache.clear();
  IR::Function RHSF;
  if (!translateRoot(RHS, nullptr, RHSF, RExprCache)) {
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
    std::ostringstream os;
    os << errs << "\n";
    llvm::errs() << os.str();
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
                         bool Negate) override {
    llvm::report_fatal_error("Do not call");
    return "";
  }
  std::string GetExprStr(const souper::BlockPCs & BPCs,
                         const std::vector<souper::InstMapping> & PCs,
                         souper::InstMapping Mapping,
                         std::vector<souper::Inst *> * ModelVars,
                         bool Negate) override {
    llvm::report_fatal_error("Do not call");
    return "";
  }
};

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
      return translateDataflowFacts(I, F, ExprCache);
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
      return true;
    }

    #define BINOP(SOUPER, ALIVE) case souper::Inst::SOUPER: {    \
      ExprCache[I] = Builder.binOp(t, Name, ExprCache[I->Ops[0]],\
      ExprCache[I->Ops[1]], IR::BinOp::ALIVE);                   \
      return true;                                               \
    }

    #define BINOPF(SOUPER, ALIVE, W) case souper::Inst::SOUPER: {\
      ExprCache[I] = Builder.binOp(t, Name, ExprCache[I->Ops[0]],\
      ExprCache[I->Ops[1]], IR::BinOp::ALIVE, IR::BinOp::W);     \
      return true;                                               \
    }

    BINOP(Add, Add);
    BINOPF(AddNSW, Add, NSW);
    BINOPF(AddNUW, Add, NUW);
    BINOPF(AddNW, Add, NSWNUW);
    BINOP(Sub, Sub);
    BINOPF(SubNSW, Sub, NSW);
    BINOPF(SubNUW, Sub, NUW);
    BINOPF(SubNW, Sub, NSWNUW);
    BINOP(Mul, Mul);
    BINOPF(MulNSW, Mul, NSW);
    BINOPF(MulNUW, Mul, NUW);
    BINOPF(MulNW, Mul, NSWNUW);
    BINOP(And, And);
    BINOP(Or, Or);
    BINOP(Xor, Xor);
    BINOP(Shl, Shl);
    BINOPF(ShlNSW, Shl, NSW);
    BINOPF(ShlNUW, Shl, NUW);
    BINOPF(ShlNW, Shl, NSWNUW);
    BINOP(LShr, LShr);
    BINOPF(LShrExact, LShr, Exact);
    BINOP(AShr, AShr);
    BINOPF(AShrExact, AShr, Exact);
    BINOP(Cttz, Cttz);
    BINOP(Ctlz, Ctlz);
    BINOP(URem, URem);
    BINOP(SRem, SRem);
    BINOP(UDiv, UDiv);
    BINOPF(UDivExact, UDiv, Exact);
    BINOP(SDiv, SDiv);
    BINOPF(SDivExact, SDiv, Exact);

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

bool
souper::AliveDriver::translateBlockPCs(const souper::Inst* I,
                                      IR::Function& F,
                                      souper::AliveDriver::Cache& ExprCache) {
  DummyExprBuilder EB(IC);
  EB.setBlockPCMap(BPCs);
  auto BPCCondition = EB.getBlockPCs(const_cast<Inst *>(I));
    if (BPCCondition) {
    if (!translateAndCache(BPCCondition, F, ExprCache)) {
      return false;
    }
    FunctionBuilder Builder(F);
    Builder.assume(ExprCache[BPCCondition]);
    return true;
  } else {
    return false;
  }
}

IR::Type &souper::AliveDriver::getType(int n) {
  if (TypeCache.find(n) == TypeCache.end()) {
    TypeCache[n] = new IR::IntType("i" + std::to_string(n), n);
  }
  return *TypeCache[n];
}

bool souper::isTransformationValid(souper::Inst* LHS, souper::Inst* RHS,
                                   const BlockPCs &BPCs,
                                   const std::vector<InstMapping> &PCs,
                                   InstContext &IC) {
  AliveDriver Verifier(LHS, BPCs, PCs, IC);
  return Verifier.verify(RHS);
}
