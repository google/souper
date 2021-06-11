#include "souper/Infer/Z3Expr.h"

namespace z3expr {
using E = z3::expr;
using namespace z3;
E Add(E x, E y) {
  return x + y;
}
E Sub(E x, E y) {
  return x - y;
}
E Mul(E x, E y) {
  return x * y;
}
E UDiv(E x, E y) {
  return z3::udiv(x, y);
}
E SDiv(E x, E y) {
  return x / y;
}
E URem(E x, E y) {
  return z3::urem(x, y);
}
E SRem(E x, E y) {
  return z3::srem(x, y);
}
E And(E x, E y) {
  return x & y;
}
E Or(E x, E y) {
  return x | y;
}
E Xor(E x, E y) {
  return x ^ y;
}
E Shl(E x, E y) {
  return z3::shl(x, y);
}
E LShr(E x, E y) {
  return z3::lshr(x, y);
}
E AShr(E x, E y) {
  return z3::ashr(x, y);
}
E Select(E C, E T, E F) {
  return z3::ite(C == C.ctx().bv_val(1, 1), T, F);
}
E ZExt(E x, size_t W) {
  auto xW = x.get_sort().bv_size();
  return z3::zext(x, W - xW);
}
E SExt(E x, size_t W) {
  auto xW = x.get_sort().bv_size();
  return z3::sext(x, W - xW);
}
E Trunc(E x, size_t W) {
  return x.extract(W-1, 0);
}

E ToBV(E x) {
  auto &ctx = x.ctx();
  return z3::ite(x, ctx.bv_val(1, 1), ctx.bv_val(0, 1));
}

E Eq(E x, E y) {
  return ToBV(x == y);
}
E Ne(E x, E y) {
  return ToBV(x != y);
}
E Ult(E x, E y){
  return ToBV(z3::ult(x, y));
}
E Slt(E x, E y){
  return ToBV(z3::slt(x, y));
}
E Ule(E x, E y){
  return ToBV(z3::ule(x, y));
}
E Sle(E x, E y){
  return ToBV(z3::sle(x, y));
}
E CtPop(E x) {
  auto W = x.get_sort().bv_size();
  auto &ctx = x.ctx();
  auto sum = ctx.bv_val(0, W);
  for (size_t i = 0; i < W; ++i) {
    sum = sum + z3::zext(x.extract(i, i), W - 1);
  }
  return sum;
}
E Freeze(E x) {
  return x;
}

E ExtractValue(E x, size_t idx) {
  if (idx == 0) { // return value
    return x.extract(x.get_sort().bv_size() - 1, 1);
  } else { // return overflow flag
    return x.extract(0 , 0);
  }
}

E add_no_soverflow(E x, E y) {
  return sext(x, 1) + sext(y, 1) == sext(x + y, 1);
}

E add_no_uoverflow(E x, E y) {
  auto bw = x.get_sort().bv_size();
  return (zext(x, 1) + zext(y, 1)).extract(bw, bw) == 0;
}

E sub_no_soverflow(E x, E y) {
  return sext(x, 1) - sext(y, 1) == sext((x - y), 1);
}

E sub_no_uoverflow(E x, E y) {
  auto bw = x.get_sort().bv_size();
  return (zext(x, 1) - zext(y, 1)).extract(bw, bw) == 0;
}

E mul_no_soverflow(E x, E y) {
  auto bw = x.get_sort().bv_size();
  return sext(x, bw) * sext(y, bw) == sext((x * y), bw);
}

E mul_no_uoverflow(E x, E y) {
  auto bw = x.get_sort().bv_size();
  return (zext(x, bw) * zext(y, bw)).extract(2*bw - 1, bw) == 0;
}

E shl_no_soverflow(E x, E y) {
  return ashr(shl(x, y), y) == x;
}

E shl_no_uoverflow(E x, E y) {
  return lshr(shl(x, y), y) == x;
}

E ToIBV(E x) {
  auto &ctx = x.ctx();
  return z3::ite(x, ctx.bv_val(0, 1), ctx.bv_val(1, 1));
}

E SAddWithOverflow(E x, E y) {
  return z3::concat(x + y, ToIBV(add_no_soverflow(x, y)));
}

E UAddWithOverflow(E x, E y) {
  return z3::concat(x + y, ToIBV(add_no_uoverflow(x, y)));
}

E SSubWithOverflow(E x, E y) {
  return z3::concat(x - y, ToIBV(sub_no_soverflow(x, y)));
}

E USubWithOverflow(E x, E y) {
  return z3::concat(x - y, ToIBV(sub_no_uoverflow(x, y)));
}

E SMulWithOverflow(E x, E y) {
  return z3::concat(x * y, ToIBV(mul_no_soverflow(x, y)));
}

E UMulWithOverflow(E x, E y) {
  return z3::concat(x * y, ToIBV(mul_no_uoverflow(x, y)));
}

// following implementations were borrowed from alive2 codebase
E BSwap(E x) {
  auto nbits = x.get_sort().bv_size();
  constexpr unsigned bytelen = 8;
  assert(nbits % (bytelen * 2) == 0);
  E res = x.extract(bytelen - 1, 0);
  for (unsigned i = 1; i < nbits / bytelen; i++) {
    res = z3::concat(res, x.extract((i + 1) * bytelen - 1, i * bytelen));
  }
  return res;
}
E Cttz(E x) {
  auto srt = x.get_sort();
  auto result = x.ctx().bv_val(0, srt.bv_size());
  for (int i = srt.bv_size() - 1; i >= 0; --i) {
    result = z3::ite(x.extract(i, i) == x.ctx().bv_val(1, 1),
                     x.ctx().bv_val(i, srt.bv_size()), result);
  }
  return result;
}
E Ctlz(E x) {
  auto nbits = x.get_sort().bv_size();
  auto result = x.ctx().bv_val(nbits, nbits);
  for (unsigned i = 0; i < nbits; ++i) {
    result = z3::ite(x.extract(i, i) == x.ctx().bv_val(1, 1),
                     x.ctx().bv_val(nbits - 1 - i, nbits), result);
  }
  return result;
}
E BitReverse(E x) {
  auto nbits = x.get_sort().bv_size();

  E res = x.extract(0, 0);
  for (unsigned i = 1; i < nbits; ++i) {
    res = concat(res, x.extract(i, i));
  }

  return res;
}
E FShl(E a, E b, E c) {
  auto width = a.ctx().bv_val(a.get_sort().bv_size(), a.get_sort().bv_size());
  E c_mod_width = z3::urem(c, width);
  return shl(a, c_mod_width) | z3::lshr(b, width - c_mod_width);
}

E FShr(E a, E b, E c) {
  auto width = a.ctx().bv_val(a.get_sort().bv_size(), a.get_sort().bv_size());
  E c_mod_width = z3::urem(c, width);
  return shl(a , (width - c_mod_width)) | z3::lshr(b, c_mod_width);
}

static E IntSMin(unsigned bits, z3::context &ctx) {
  E v = ctx.bv_val(1, 1);
  if (bits > 1)
    v = z3::concat(v, ctx.bv_val(0, bits - 1));
  return v;
}

static E IntSMax(unsigned bits, z3::context &ctx) {
  E v = ctx.bv_val(0, 1);
  if (bits > 1)
    v = z3::concat(v, ctx.bv_val(-1, bits - 1));
  return v;
}

static E IntUMax(unsigned bits, z3::context &ctx) {
  return ctx.bv_val(-1, bits);
}

E SAddSat(E x, E y) {
  E add_ext = z3::sext(x, 1) + z3::sext(y, 1);
  auto bw = x.get_sort().bv_size();
  auto min = IntSMin(bw, x.ctx());
  auto max = IntSMax(bw, x.ctx());
  return z3::ite(z3::sle(add_ext, z3::sext(min, 1)),
              min,
              z3::ite(z3::sge(add_ext, z3::sext(max, 1)),
                   max,
                   x + y));
}

E UAddSat(E x, E y) {
  return z3::ite(z3::bvadd_no_overflow(x, y, false),
              x + y,
              IntUMax(x.get_sort().bv_size(), x.ctx()));
}

E SSubSat(E x, E y){
  E sub_ext = z3::sext(x, 1) - sext(y, 1);
  auto bw = x.get_sort().bv_size();
  auto min = IntSMin(bw, x.ctx());
  auto max = IntSMax(bw, x.ctx());
  return z3::ite(z3::sle(sub_ext, z3::sext(min, 1)),
              min,
              z3::ite(z3::sge(sub_ext, z3::sext(max, 1)),
                   max,
                   x - y));
}

E USubSat(E x, E y) {
  return z3::ite(uge(y, x),
              x.ctx().bv_val(0, x.get_sort().bv_size()),
              x - y);
}

E sdiv_exact(E x, E y) {
  return x / y * y == x;
}

E udiv_exact(E x, E y) {
  return udiv(x, y) * y == x;
}

E ashr_exact(E x, E y) {
  return (shl(ashr(x, y),  y)) == x;
}

E lshr_exact(E x, E y) {
  return (shl(lshr(x, y), y) ) == x;
}

}
