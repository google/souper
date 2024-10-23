#include "souper/Infer/Z3Driver.h"
#include "souper/Infer/Z3Expr.h"


extern unsigned DebugLevel;

namespace souper {

// True result means unsat, false means sat
// TODO: handle unknown
bool Z3Driver::verify(Inst *RHS, Inst *RHSAssumptions, bool Invert) {

  auto Cond = EB->GetCandidateExprForReplacement(BPCs, PCs, InstMapping(LHS, RHS),
                                     RHSAssumptions, Invert, false);
  Translate(Cond);
  SolverPushRAII SR(Solver);
  Solver.add(Get(Cond) == ctx.bv_val(0, 1));

  if (DebugLevel >= 4) {
    llvm::errs() << "Solver assertions:\n";
    for (auto &&E : Solver.assertions()) {
      std::cerr << E << "\n";
    }
  }

  if (Solver.check() == z3::unsat) {
    return true;
  } else {
    Model = Solver.get_model();
    return false;
  }
}

bool isTransformationValidZ3(souper::Inst *LHS, souper::Inst *RHS,
                           const std::vector<InstMapping> &PCs,
                           const souper::BlockPCs &BPCs,
                           InstContext &IC, unsigned Timeout) {

  std::vector<Inst *> Vars;
  findVars(RHS, Vars);
  Z3Driver Verifier(LHS, PCs, IC, BPCs,  Vars, Timeout);
  if (!Verifier.verify(RHS, nullptr)) {
    return false;
  } else {
    return true;
  }
}

void Z3Driver::genExtraPreds(souper::Inst *I) {
  if (I->K == Inst::Kind::UDiv || I->K == Inst::Kind::SDiv
      || I->K == Inst::Kind::SDivExact || I->K == Inst::Kind::UDivExact
      || I->K == Inst::Kind::URem || I->K == Inst::Kind::SRem) {
    ExtraPreds.push_back(Get(I->Ops[1]) != ctx.bv_val(0, I->Width));
  }

  if (I->K == Inst::Kind::Shl || I->K == Inst::Kind::LShr
      || I->K == Inst::Kind::AShr || I->K == Inst::Kind::AShrExact
      || I->K == Inst::Kind::LShrExact) {
    ExtraPreds.push_back(z3::ult(Get(I->Ops[1]), ctx.bv_val(I->Width, I->Width)));
  }

  if (I->K == Inst::Kind::AddNSW || I->K == Inst::Kind::AddNW) {
    ExtraPreds.push_back(z3expr::add_no_soverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }

  if (I->K == Inst::Kind::AddNUW || I->K == Inst::Kind::AddNW) {
    ExtraPreds.push_back(z3expr::add_no_uoverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }

  if (I->K == Inst::Kind::SubNSW || I->K == Inst::Kind::SubNW) {
    ExtraPreds.push_back(z3expr::sub_no_soverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }
  if (I->K == Inst::Kind::SubNUW || I->K == Inst::Kind::SubNW) {
    ExtraPreds.push_back(z3expr::sub_no_uoverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }

  if (I->K == Inst::Kind::MulNSW || I->K == Inst::Kind::MulNW) {
    ExtraPreds.push_back(z3expr::mul_no_soverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }
  if (I->K == Inst::Kind::MulNUW || I->K == Inst::Kind::MulNW) {
    ExtraPreds.push_back(z3expr::mul_no_uoverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }

  if (I->K == Inst::Kind::ShlNSW || I->K == Inst::Kind::ShlNW) {
    ExtraPreds.push_back(z3expr::shl_no_soverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }
  if (I->K == Inst::Kind::ShlNUW || I->K == Inst::Kind::ShlNW) {
    ExtraPreds.push_back(z3expr::shl_no_uoverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }

  if (I->K == Inst::Kind::UDivExact) {
    ExtraPreds.push_back(z3expr::udiv_exact(Get(I->Ops[0]), Get(I->Ops[1])));
  }
  if (I->K == Inst::Kind::SDivExact) {
    ExtraPreds.push_back(z3expr::sdiv_exact(Get(I->Ops[0]), Get(I->Ops[1])));
  }

  if (I->K == Inst::Kind::AShrExact) {
    ExtraPreds.push_back(z3expr::ashr_exact(Get(I->Ops[0]), Get(I->Ops[1])));
  }
  if (I->K == Inst::Kind::LShrExact) {
    ExtraPreds.push_back(z3expr::lshr_exact(Get(I->Ops[0]), Get(I->Ops[1])));
  }
}

}
