#ifndef SOUPER_Z3_DRIVER_H
#define SOUPER_Z3_DRIVER_H
#include "souper/Infer/Verification.h"
#include "souper/Infer/Z3Expr.h"
#include "souper/Inst/Inst.h"
#include "souper/Extractor/ExprBuilder.h"
#include <optional>
namespace souper {

class Z3Driver {
public:
  Z3Driver(Inst *LHS_, const std::vector<InstMapping> PCs_, InstContext &IC_, BlockPCs BPCs_,
           std::vector<Inst *> ExtraInputs = {}, unsigned Timeout = 100000)
    : LHS(LHS_), PCs(PCs_), IC(IC_),
      TranslatedExprs(ctx), ExtraPreds(ctx), Solver(ctx), Model(ctx) {

    EB = createKLEEBuilder(IC);
    // TODO: Preprocessing, solver keepalive
    z3::params p(ctx);

    Z3_global_param_set("timeout", "1000");
    p.set(":logic", "QF_BV");


// Following list is used by alive2
//    "simplify",
//    "propagate-values",
//    "simplify",
//    "elim-uncnstr",
//    "qe-light",
//    "simplify",
//    "elim-uncnstr",
//    "reduce-args",
//    "qe-light",
//    "simplify",
//    "smt"

    Solver = (z3::tactic(ctx, "simplify") &
              z3::tactic(ctx, "propagate-values") &
              z3::tactic(ctx, "simplify") &
              z3::tactic(ctx, "elim-uncnstr") &
              z3::tactic(ctx, "qe-light") &
              z3::tactic(ctx, "simplify") &
              z3::tactic(ctx, "elim-uncnstr") &
              z3::tactic(ctx, "reduce-args") &
              z3::tactic(ctx, "qe-light") &
              z3::tactic(ctx, "bit-blast") &
              z3::tactic(ctx, "smt")).mk_solver();
    // Hangs for 1-2% of the problems without adding bit blast.
    // OTOH adding bit blast makes it not much faster than the klee backend.
    // TODO: Investigate if it makes sense to add this option for some specific problems.

    Solver.set(p);

    InstNumbers = 201;
    //201 is chosen arbitrarily.
  }

  // True result means unsat, false means sat
  // TODO: handle unknown
  bool verify(Inst *RHS, Inst *RHSAssumptions = nullptr, bool Invert = false);

  std::optional<llvm::APInt> getModelVal(Inst *I) {
    auto Expr = Model.eval(Get(I));
    if (Expr.is_numeral()) {
      return llvm::APInt(Expr.get_sort().bv_size(), Expr.get_decimal_string(0), 10);
    } else {
      return {};
    }
  }

private:
  Inst *LHS;
  Inst *Precondition = nullptr;
  InstContext &IC;
  const std::vector<InstMapping> PCs;
  std::map<Block *, std::map<unsigned, std::vector<InstMapping>>> BPCMap;
  BlockPCs BPCs;
  std::map<const Inst *, std::string> NamesCache;
  std::map<Inst *, size_t> ExprCache;
  z3::context ctx;
  z3::expr_vector TranslatedExprs;
  z3::expr_vector ExtraPreds;
  z3::solver Solver;
  z3::model Model;

  std::unique_ptr<ExprBuilder> EB;

  struct SolverPushRAII {
    SolverPushRAII(z3::solver &S_) : S(S_) {S.push();}
    ~SolverPushRAII() {S.pop();}
    z3::solver &S;
  };

  bool isCached(Inst *I) {
    return ExprCache.find(I) != ExprCache.end();
  }

  z3::expr Get(Inst *I) {
    return TranslatedExprs[ExprCache[I]];
  }
  void Put(Inst *I, z3::expr E) {
    TranslatedExprs.push_back(E);
    ExprCache[I] = TranslatedExprs.size() - 1;
  }

  void AddConstraint(Inst *I) {
    Translate(I);
    Solver.add(Get(I) == ctx.bv_val(1, 1));
  }

  z3::expr getPhiArgConstraint(Block *B, unsigned idx) {
    souper::Inst *Cond = nullptr;
    for (auto Mapping : BPCMap[B][idx]) {
      auto Cur = IC.getInst(Inst::Kind::Eq, 1, {Mapping.LHS, Mapping.RHS});
      if (!Cond) {
        Cond = Cur;
      } else {
        Cond = IC.getInst(Inst::Kind::And, 1, {Cond, Cur});
      }
    }

    if (!Cond) {
      return ctx.bool_val(true);
    }

    Translate(Cond);
    auto Expr = Get(Cond);
    if (Expr.get_sort().is_bv()) {
      // i1 to bool. TODO: investigate if there is a more efficient way of doing this.
      auto &ctx = Expr.ctx();
      Expr = z3::ite(Expr == ctx.bv_val(1, 1), ctx.bool_val(true), ctx.bool_val(false));
      Put(Cond, Expr);
    }
    return Expr;
  }

  int InstNumbers;

  void genExtraPreds(souper::Inst *I);

  bool Translate(souper::Inst *I) {
    if (!I) {
      return false;
    }
    // unused translation; this is souper's internal instruction to represent overflow instructions
    if (souper::Inst::isOverflowIntrinsicSub(I->K)) {
      return true;
    }

    if (isCached(I)) {
      return true;
    }

    auto Ops = I->Ops;
    if (souper::Inst::isOverflowIntrinsicMain(I->K)) {
      Ops = Ops[0]->Ops;
    }

    for (auto &&Op : Ops) {
      if (!Translate(Op)) {
        return false;
      }
      if (I->K == Inst::ExtractValue) {
        break; // Break after translating main arg, idx is handled separately.
      }
    }

    std::string Name;
    if (NamesCache.find(I) != NamesCache.end()) {
      Name = NamesCache[I];
    } else if (I->Name != "") {
      if (I->SynthesisConstID != 0) {
        Name = "%" + souper::ReservedConstPrefix + std::to_string(I->SynthesisConstID);
      } else {
        Name = "%var_" + I->Name;
      }
    } else {
      Name = "%" + std::to_string(InstNumbers++);
    }
    NamesCache[I] = Name;

    auto W = I->Width;
    genExtraPreds(I);
    switch (I->K) {
      case souper::Inst::Var: {
        Put(I, ctx.bv_const(Name.c_str(), W));
        auto DFC = getDataflowConditions(I, IC);
        if (DFC) {
          AddConstraint(DFC);
        }
        return true;
      }
      case souper::Inst::Hole: {
        llvm::report_fatal_error("Holes unimplemented in Z3Driver.");
      }
      case souper::Inst::Const: {
        Put(I, ctx.bv_val(I->Val.toString(10, false).c_str(), W));
        // inefficient?
        return true;
      }

      case souper::Inst::Phi: {
        auto Var = ctx.bv_const(Name.c_str(), I->Width);
        auto Constraint = ((Var == Get(I->Ops[0])) && getPhiArgConstraint(I->B, 0));
        for (size_t i = 0; i < I->Ops.size(); ++i) {
          Constraint = Constraint ||
              ((Var == Get(I->Ops[i])) && getPhiArgConstraint(I->B, i));
        }
        Solver.add(Constraint);
        Put(I, Var);
        return true;
      }

      case souper::Inst::ExtractValue: {
        unsigned idx = I->Ops[1]->Val.getLimitedValue();
        assert(idx <= 1 && "Only extractvalue with overflow instructions are supported.");
        Put(I, z3expr::ExtractValue(Get(I->Ops[0]), idx));
        return true;
      }

      #define UNOP(SOUPER, Z3) case souper::Inst::SOUPER: {       \
        Put(I, z3expr::Z3(Get(Ops[0])));                          \
        return true;                                              \
      }
      #define UNOPC(SOUPER, Z3) case souper::Inst::SOUPER: {      \
        Put(I, z3expr::Z3(Get(Ops[0]), I->Width));                \
        return true;                                              \
      }
      #define BINOP(SOUPER, Z3) case souper::Inst::SOUPER: {      \
        Put(I, z3expr::Z3(Get(Ops[0]), Get(Ops[1])));             \
        return true;                                              \
      }
      #define TERNOP(SOUPER, Z3) case souper::Inst::SOUPER: {     \
        Put(I, z3expr::Z3(Get(Ops[0]), Get(Ops[1]), Get(Ops[2])));\
        return true;                                              \
      }

      UNOP(Freeze, Freeze); UNOP(CtPop, CtPop); UNOP(BSwap, BSwap);
      UNOP(BitReverse, BitReverse); UNOP(Cttz, Cttz); UNOP(Ctlz, Ctlz);

      UNOPC(ZExt, ZExt); UNOPC(SExt, SExt); UNOPC(Trunc, Trunc);

      BINOP(Add, Add); BINOP(AddNSW, Add); BINOP(AddNUW, Add); BINOP(AddNW, Add);
      BINOP(Sub, Sub); BINOP(SubNSW, Sub); BINOP(SubNUW, Sub); BINOP(SubNW, Sub);
      BINOP(Mul, Mul); BINOP(MulNSW, Mul); BINOP(MulNUW, Mul); BINOP(MulNW, Mul);
      BINOP(Shl, Shl); BINOP(ShlNSW, Shl); BINOP(ShlNUW, Shl); BINOP(ShlNW, Shl);
      BINOP(And, And); BINOP(Or, Or); BINOP(Xor, Xor);
      BINOP(LShr, LShr); BINOP(LShrExact, LShr); BINOP(AShr, AShr); BINOP(AShrExact, AShr);
      BINOP(URem, URem); BINOP(SRem, SRem); BINOP(UDiv, UDiv); BINOP(UDivExact, UDiv);
      BINOP(SDiv, SDiv); BINOP(SDivExact, SDiv); BINOP(SAddSat, SAddSat);
      BINOP(UAddSat, SAddSat); BINOP(SSubSat, SSubSat); BINOP(USubSat, USubSat);
      BINOP(SAddWithOverflow, SAddWithOverflow); BINOP(UAddWithOverflow, UAddWithOverflow);
      BINOP(SSubWithOverflow, SSubWithOverflow); BINOP(USubWithOverflow, USubWithOverflow);
      BINOP(SMulWithOverflow, SMulWithOverflow); BINOP(UMulWithOverflow, UMulWithOverflow);
      BINOP(Eq, Eq); BINOP(Ne, Ne); BINOP(Ule, Ule);
      BINOP(Ult, Ult); BINOP(Sle, Sle); BINOP(Slt, Slt);

      TERNOP(Select, Select); TERNOP(FShl, FShl); TERNOP(FShr, FShr);

      default: llvm::report_fatal_error("Unimplemented instruction.");
    }
  }
};


bool isTransformationValidZ3(souper::Inst *LHS, souper::Inst *RHS,
                             const std::vector<InstMapping> &PCs,
                             const souper::BlockPCs &BPCs,
                             InstContext &IC, unsigned Timeout = 10000);

}

#endif
