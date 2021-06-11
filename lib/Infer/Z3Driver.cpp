#include "souper/Infer/Z3Driver.h"
#include "souper/Infer/Z3Expr.h"

extern unsigned DebugLevel;

namespace souper {
bool isTransformationValidZ3(souper::Inst *LHS, souper::Inst *RHS,
                           const std::vector<InstMapping> &PCs,
                           const souper::BlockPCs &BPCs,
                           InstContext &IC, unsigned Timeout) {
  Inst *Ante = IC.getConst(llvm::APInt(1, true));
  for (auto PC : PCs ) {
    Inst *Eq = IC.getInst(Inst::Eq, 1, {PC.LHS, PC.RHS});
    Ante = IC.getInst(Inst::And, 1, {Ante, Eq});
  }

//  auto Goals = explodePhis(IC, {LHS, RHS, Ante, BPCs});
//  // ^ Explanation in AliveDriver.cpp

//  if (DebugLevel > 3)
//    llvm::errs() << "Number of sub-goals : " << Goals.size() << "\n";
//  for (const auto &Goal : Goals) {
//    if (DebugLevel > 3) {
//      llvm::errs() << "Goal:\n";
//      ReplacementContext RC;
//      RC.printInst(Goal.LHS, llvm::errs(), true);
//      llvm::errs() << "\n------\n";
//    }
//    std::vector<Inst *> Vars;
//    findVars(Goal.RHS, Vars);
//    Z3Driver Verifier(Goal.LHS, Goal.Pre, IC, BPCs,  Vars, Timeout);
//    if (!Verifier.verify(Goal.RHS, Goal.Pre))
//      return false;
//  }

  std::vector<Inst *> Vars;
  findVars(RHS, Vars);
  Z3Driver Verifier(LHS, Ante, IC, BPCs,  Vars, Timeout);
  if (!Verifier.verify(RHS, Ante)) {
    return false;
  } else {
    return true;
  }
}

void Z3Driver::addExtraPreds(souper::Inst *I) {
  if (I->K == Inst::Kind::UDiv || I->K == Inst::Kind::SDiv
      || I->K == Inst::Kind::SDivExact || I->K == Inst::Kind::UDivExact
      || I->K == Inst::Kind::URem || I->K == Inst::Kind::SRem) {
    Solver.add(Get(I->Ops[1]) != ctx.bv_val(0, I->Width));
  }

  if (I->K == Inst::Kind::Shl || I->K == Inst::Kind::LShr
      || I->K == Inst::Kind::AShr || I->K == Inst::Kind::AShrExact
      || I->K == Inst::Kind::LShrExact) {
    Solver.add(z3::ult(Get(I->Ops[1]), ctx.bv_val(I->Width, I->Width)));
  }

  if (I->K == Inst::Kind::AddNSW || I->K == Inst::Kind::AddNW) {
    Solver.add(z3expr::add_no_soverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }

  if (I->K == Inst::Kind::AddNUW || I->K == Inst::Kind::AddNW) {
    Solver.add(z3expr::add_no_uoverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }

  if (I->K == Inst::Kind::SubNSW || I->K == Inst::Kind::SubNW) {
    Solver.add(z3expr::sub_no_soverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }
  if (I->K == Inst::Kind::SubNUW || I->K == Inst::Kind::SubNW) {
    Solver.add(z3expr::sub_no_uoverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }

  if (I->K == Inst::Kind::MulNSW || I->K == Inst::Kind::MulNW) {
    Solver.add(z3expr::mul_no_soverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }
  if (I->K == Inst::Kind::MulNUW || I->K == Inst::Kind::MulNW) {
    Solver.add(z3expr::mul_no_uoverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }

  if (I->K == Inst::Kind::ShlNSW || I->K == Inst::Kind::ShlNW) {
    Solver.add(z3expr::shl_no_soverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }
  if (I->K == Inst::Kind::ShlNUW || I->K == Inst::Kind::ShlNW) {
    Solver.add(z3expr::shl_no_uoverflow(Get(I->Ops[0]), Get(I->Ops[1])));
  }

  if (I->K == Inst::Kind::UDivExact) {
    Solver.add(z3expr::udiv_exact(Get(I->Ops[0]), Get(I->Ops[1])));
  }
  if (I->K == Inst::Kind::SDivExact) {
    Solver.add(z3expr::sdiv_exact(Get(I->Ops[0]), Get(I->Ops[1])));
  }

  if (I->K == Inst::Kind::AShrExact) {
    Solver.add(z3expr::ashr_exact(Get(I->Ops[0]), Get(I->Ops[1])));
  }
  if (I->K == Inst::Kind::LShrExact) {
    Solver.add(z3expr::lshr_exact(Get(I->Ops[0]), Get(I->Ops[1])));
  }
}

}
