#ifndef Z3_EXPR_H
#define Z3_EXPR_H
#include "z3++.h"
namespace z3expr {

z3::expr Add(z3::expr x, z3::expr y);

z3::expr Sub(z3::expr x, z3::expr y);

z3::expr Mul(z3::expr x, z3::expr y);

z3::expr UDiv(z3::expr x, z3::expr y);

z3::expr SDiv(z3::expr x, z3::expr y);

z3::expr URem(z3::expr x, z3::expr y);

z3::expr SRem(z3::expr x, z3::expr y);

z3::expr And(z3::expr x, z3::expr y);

z3::expr Or(z3::expr x, z3::expr y);

z3::expr Xor(z3::expr x, z3::expr y);

z3::expr Shl(z3::expr x, z3::expr y);

z3::expr LShr(z3::expr x, z3::expr y);

z3::expr AShr(z3::expr x, z3::expr y);

z3::expr Select(z3::expr C, z3::expr T, z3::expr F);

z3::expr ZExt(z3::expr x, size_t W);

z3::expr SExt(z3::expr x, size_t W);

z3::expr Trunc(z3::expr x, size_t W);

z3::expr Eq(z3::expr x, z3::expr y);

z3::expr Ne(z3::expr x, z3::expr y);

z3::expr Ult(z3::expr x, z3::expr y);

z3::expr Slt(z3::expr x, z3::expr y);

z3::expr Ule(z3::expr x, z3::expr y);

z3::expr Sle(z3::expr x, z3::expr y);

z3::expr CtPop(z3::expr x);

z3::expr Freeze(z3::expr x);

z3::expr ExtractValue(z3::expr x, size_t idx);

z3::expr SAddWithOverflow(z3::expr x, z3::expr y);

z3::expr UAddWithOverflow(z3::expr x, z3::expr y);

z3::expr SSubWithOverflow(z3::expr x, z3::expr y);

z3::expr USubWithOverflow(z3::expr x, z3::expr y);

z3::expr SMulWithOverflow(z3::expr x, z3::expr y);

z3::expr UMulWithOverflow(z3::expr x, z3::expr y);

z3::expr BSwap(z3::expr x);

z3::expr Cttz(z3::expr x);

z3::expr Ctlz(z3::expr x);

z3::expr BitReverse(z3::expr x);

z3::expr FShl(z3::expr a, z3::expr b, z3::expr c);

z3::expr FShr(z3::expr a, z3::expr b, z3::expr c);

z3::expr SAddSat(z3::expr x, z3::expr y);

z3::expr UAddSat(z3::expr x, z3::expr y);

z3::expr SSubSat(z3::expr x, z3::expr y);

z3::expr USubSat(z3::expr x, z3::expr y);

z3::expr add_no_soverflow(z3::expr x, z3::expr y);

z3::expr add_no_uoverflow(z3::expr x, z3::expr y);

z3::expr sub_no_soverflow(z3::expr x, z3::expr y);

z3::expr sub_no_uoverflow(z3::expr x, z3::expr y);

z3::expr mul_no_soverflow(z3::expr x, z3::expr y);

z3::expr mul_no_uoverflow(z3::expr x, z3::expr y);

z3::expr shl_no_soverflow(z3::expr x, z3::expr y);

z3::expr shl_no_uoverflow(z3::expr x, z3::expr y);

z3::expr sdiv_exact(z3::expr x, z3::expr y);

z3::expr udiv_exact(z3::expr x, z3::expr y);

z3::expr ashr_exact(z3::expr x, z3::expr y);

z3::expr lshr_exact(z3::expr x, z3::expr y);

z3::expr ToBV(z3::expr x);

z3::expr ToIBV(z3::expr x); //inverted
}
#endif
