#include "Verification.h"
#include <iostream>

namespace souper {
using namespace z3;
bool AbstractDomain::Verify(std::string OpName, size_t Width) {
  solver s(ctx);

  params p(ctx);
  p.set("mbqi", true);
//   p.set("bv_literals", false);
  s.set(p);
  
  auto abstractx = ctx.bv_const("ax", Width);
  auto abstracty = ctx.bv_const("ay", Width);
  
  auto concretex = ctx.bv_const("cx", Width);
  auto concretey = ctx.bv_const("cy", Width);
  

  auto concrete_result = Ops[OpName]->ApplyConcrete({concretex, concretey});
  auto abstract_result = Ops[OpName]->ApplyAbstract({abstractx, abstracty});
  
  s.add(MembershipTest(concretex, abstractx, Width)); // cx \in \gamma(ax) 
  s.add(MembershipTest(concretey, abstracty, Width)); // cy \in \gamma(ay)
  s.add(!MembershipTest(concrete_result,
                        abstract_result, Width)); // cr \notin \gamma(ar)  
  
  // Does there exist (cx, cy, ax, ay) such that the 
  // Op(cx, cy) \notin \gamma(TF(ax, ay))
  // BUT cx \in \gamma(ax), and cy \in \gamma(ay)?
  
  if (!s.check()) {
    // could not find counterexample
    std::cout << "Valid.";
    return true;
  } else {
    std::cout << "Invalid.\t";
    auto &&m = s.get_model();
    std::cout << "ax:" << m.eval(abstractx) << "\t"
        << "ay:" << m.eval(abstracty) << "\t"
        << "ar:" << m.eval(abstract_result) << "\t"
        << "cx:" << m.eval(concretex) << "\t"
        << "cy:" << m.eval(concretey) << "\t"
        << "cr:" << m.eval(concrete_result) << "\t";
    return false;
  }
}
}
