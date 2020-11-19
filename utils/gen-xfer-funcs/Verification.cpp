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
  
  auto concretex = ctx.bv_const("cx", Width);
  auto concretey = ctx.bv_const("cy", Width);

  std::vector<expr> abstract_inputs;
  for (int i =0; i < NumComponents; ++i) {
    expr abstractx = ctx.bv_const(("ax_"+std::to_string(i)).c_str(), Width);
    expr abstracty = ctx.bv_const(("ay_"+std::to_string(i)).c_str(), Width);

    s.add(ComposedMembershipTest(concretex, abstractx, Width, i)); // cx \in \gamma(ax)

    s.add(ComposedMembershipTest(concretey, abstracty, Width, i)); // cy \in \gamma(ay)

    abstract_inputs.push_back(abstractx);
    abstract_inputs.push_back(abstracty);
  }
  
  auto concrete_result = Ops[OpName]->ApplyConcrete({concretex, concretey});

  auto abstract_result = Ops[OpName]->ApplyAbstract(abstract_inputs);

  s.add(!ComposedMembershipTest(
    concrete_result, abstract_result, Width, /*idx*/0));
  // cr \notin \gamma(ar)
  
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
    std::cout
        << "ar:" << m.eval(abstract_result) << "\t"
        << "cx:" << m.eval(concretex) << "\t"
        << "cy:" << m.eval(concretey) << "\t"
        << "cr:" << m.eval(concrete_result) << "\t"
        << "ar:(";
    for (auto e : abstract_inputs) {
      std::cout << m.eval(e) << ' ';
    }
    std::cout << ")\t";

    return false;
  }
}
}
