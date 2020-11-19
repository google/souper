#ifndef SOUPER_XFER_VERIFY
#define SOUPER_XFER_VERIFY

#include <z3++.h>
#include <functional>
#include <unordered_map>
#include <memory>
#include <vector>

namespace souper {  
using z3::expr;
using z3::context;
using BinF = std::function<expr(expr, expr)>;
using UnF = std::function<expr(expr)>;
using NF = std::function<expr(std::vector<expr>)>;

struct Op {
  virtual expr ApplyConcrete(std::vector<expr> Input) = 0;
  virtual expr ApplyAbstract(std::vector<expr> Input) = 0;
};

struct BinOp : public Op {
  BinOp(BinF C, NF A) : Concrete(C), TF(A) {};
  BinF Concrete;
  NF TF;
  
  expr ApplyConcrete(std::vector<expr> Input) override {
    return Concrete(Input[0], Input[1]);
  }
  
  expr ApplyAbstract(std::vector<expr> Input) override {
    return TF(Input);
  }
};

// Currently unused. Verification assumes binary ops
// TODO Fix
struct UnOp : public Op {
  UnF Concrete;
  UnF TF;
  
  expr ApplyConcrete(std::vector<expr> Input) override {
    return Concrete(Input[0]);
  }
  
  expr ApplyAbstract(std::vector<expr> Input) override {
    return TF(Input[0]);
  }
};

struct AbstractDomain {
  AbstractDomain(context& ctx_, size_t NumComponents_ = 1)
    : ctx(ctx_), NumComponents(NumComponents_) {}

  // concrete \in \gamma(abstract)?
  virtual expr MembershipTest(expr Concrete,
                              expr Abstract,
                              size_t Width) = 0;


  virtual expr ComposedMembershipTest(expr Concrete,
                              expr Abstract,
                              size_t Width, size_t idx) {
    assert (NumComponents == 1 && "Unexpected call to default MT.");
    return MembershipTest(Concrete, Abstract, Width);
  }

  bool Verify(std::string OpName, size_t Width);
  
  context &ctx;
  std::unordered_map<std::string, std::shared_ptr<Op>> Ops;
  size_t NumComponents;
};

struct KnownZeroDomain : public AbstractDomain {
  KnownZeroDomain(context &ctx) : AbstractDomain(ctx) {}
  expr MembershipTest(expr Concrete,
                      expr Abstract, size_t Width) override {
    // C, A -> Membership
    // 0, 0 -> T
    // 0, 1 -> T
    // 1, 0 -> T
    // 1, 1 -> F
    auto zero = ctx.bv_val(0, Width);
    return ((Abstract & Concrete) == zero);                                  
  } 
};

struct KnownOneDomain : public AbstractDomain {
  KnownOneDomain(context &ctx) : AbstractDomain(ctx) {}
  expr MembershipTest(expr Concrete,
                      expr Abstract, size_t Width) override {
    // C, A -> Membership
    // 0, 0 -> T
    // 0, 1 -> F
    // 1, 0 -> T
    // 1, 1 -> T
    // C | !A => (!C & A) == 0  
    auto zero = ctx.bv_val(0, Width);
    return ((~Concrete & Abstract) == zero);                              
  } 
};

// The first `Component` is the result domain,
// but TF's can optionally use other domain inputs.

struct ComposedDomain : public AbstractDomain {
  ComposedDomain(std::vector<std::shared_ptr<AbstractDomain>> Components_, context &ctx_)
    : Components (Components_), AbstractDomain(ctx_, Components_.size()) {}

  expr ComposedMembershipTest(expr Concrete, expr Abstract, size_t Width, size_t idx) override {
    return Components[idx]->MembershipTest(Concrete, Abstract, Width);
  }

  expr MembershipTest(expr Concrete, expr Abstract, size_t Width) override {
    return ComposedMembershipTest(Concrete, Abstract, Width, /*idx=*/0);
  }

  std::vector<std::shared_ptr<AbstractDomain>> Components;
};



// Example API usage

// int main(int argc, char **argv) {
//   context ctx;
//   KnownZeroDomain KnownZero(ctx);
//   KnownZero.Ops["and"] = std::make_unique<BinOp>(
//     [](auto a, auto b){return a & b;}, // concrete
//     [](auto a, auto b){return ~a;} // abstract
//   );
//   
//   for (int i = 1; i <= 64; ++i) {
//     KnownZero.Verify("and", i);
//     std::cout << "\n";
//   }
// }

}

#endif
