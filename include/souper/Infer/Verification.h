#ifndef SOUPER_INFER_VERIFICATION_H
#define SOUPER_INFER_VERIFICATION_H
#include "souper/Inst/Inst.h"

namespace souper {

struct RefinementProblem {
  souper::Inst *LHS;
  souper::Inst *RHS;
  souper::Inst *Pre;
  BlockPCs BPCs;

  RefinementProblem ReplacePhi(souper::InstContext &IC, std::map<Block *, size_t> &Change);

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

void collectPhis(souper::Inst *I,
                 std::map<souper::Block *, std::set<souper::Inst *>> &Phis);

std::unordered_set<RefinementProblem, RefinementProblem::Hash>
  explodePhis(InstContext &IC, RefinementProblem P);

Inst *getDataflowConditions(const Inst *I, InstContext &IC);

}
#endif
