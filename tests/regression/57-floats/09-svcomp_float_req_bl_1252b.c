// PARAM: --enable ana.float.interval
#include <assert.h>

typedef int __int32_t;
typedef unsigned int __uint32_t;

typedef union
{
    float value;
    __uint32_t word;
} ieee_float_shape_type;

/*
 * __fpclassify Categorizes floating point value arg into the following
 * categories:
 * zero, subnormal, normal, infinite, NAN, or implementation-defined category.
 * Returns One of FP_INFINITE, FP_NAN, FP_NORMAL, FP_SUBNORMAL, FP_ZERO or
 * implementation-defined type, specifying the category of arg.
 */

int __fpclassify_float(float x)
{
    __uint32_t w;

    do
    {
        ieee_float_shape_type gf_u;
        gf_u.value = (x);
        (w) = gf_u.word;
    } while (0);

    if (w == 0x00000000 || w == 0x80000000)
        return 2;
    else if ((w >= 0x00800000 && w <= 0x7f7fffff) ||
             (w >= 0x80800000 && w <= 0xff7fffff))
        return 4;
    else if ((w >= 0x00000001 && w <= 0x007fffff) ||
             (w >= 0x80000001 && w <= 0x807fffff))
        return 3;
    else if (w == 0x7f800000 || w == 0xff800000)
        return 1;
    else
        return 0;
}

float fmax_float(float x, float y)
{
    if (__fpclassify_float(x) == 0)
    {
        return y;
    }

    if (__fpclassify_float(y) == 0)
    {
        return x;
    }
    return x > y ? x : y;
}

int __signbit_float(float x)
{
    __uint32_t w;

    do
    {
        ieee_float_shape_type gf_u;
        gf_u.value = (x);
        (w) = gf_u.word;
    } while (0);

    return (w & 0x80000000) != 0;
}

int main()
{

    /* REQ-BL-1252:
     * The fmax and fmaxf procedures shall return +0 if one argument is -0
     * and the other +0.
     */

    float x = 0.0f;
    float y = -0.0f;
    float res = fmax_float(x, y);
    // y is -0 and x is +0, the result shall be +0
    assert(res == 0.0f);                                 // SUCCESS
    assert(__signbit_float(res) == 1);                   // UNKNOWN!
    assert(!(res == 0.0f && __signbit_float(res) == 0)); // UNKNOWN!

    return 0;
}
