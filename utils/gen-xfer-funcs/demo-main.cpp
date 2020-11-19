#include "Verification.h"
#include "z3++.h"
using namespace z3;
using namespace souper;

int main(int argc, char **argv) {
  context ctx;
  auto K0 = std::make_shared<KnownZeroDomain>(ctx);
  auto K1 = std::make_shared<KnownOneDomain>(ctx);

  ComposedDomain KnownZero({K0, K1}, ctx);

//   KnownZero.Ops["and"] = std::make_unique<BinOp>(
//     [](auto a, auto b){return a & b;}, // concrete
//     [](auto a){return a[0] | ~a[3];}
//     // abstract
//     // a[0] = x.KnownZero
//     // a[1] = y.KnownZero
//     // a[3] = x.KnownOne
//     // a[4] = y.KnownOne
//   );
//
//   for (int i = 1; i <= 64; ++i) {
//     KnownZero.Verify("and", i);
//     std::cout << "\n";
//   }

    KnownZero.Ops["xor"] = std::make_unique<BinOp>(
    [](auto a, auto b){return a ^ b;}, // concrete
    [](std::vector<expr> a)
    {return (a[0] & a[1]) | (a[2] & a[3]);}
    // abstract
    // a[0] = x.KnownZero
    // a[1] = y.KnownZero
    // a[3] = x.KnownOne
    // a[4] = y.KnownOne
  );

  for (int i = 1; i <= 64; ++i) {
    KnownZero.Verify("xor", i);
    std::cout << "\n";
  }

}
