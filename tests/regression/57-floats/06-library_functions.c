// PARAM: --enable ana.float.interval --enable ana.int.interval
#include <assert.h>
#include <math.h>
#include <float.h>
#include <complex.h>

int main()
{
    double dbl_min = 2.2250738585072014e-308;
    double inf = __builtin_inf();
    double nan = __builtin_nan("");

    //__buitin_isfinite(x):
    assert(__builtin_isfinite(1.0)); // SUCCESS
    assert(__builtin_isfinite(inf)); // UNKNOWN
    assert(__builtin_isfinite(nan)); // UNKNOWN

    //__buitin_isinf(x):
    assert(__builtin_isinf(1.0)); // FAIL
    assert(__builtin_isinf(inf)); // UNKNOWN
    assert(__builtin_isinf(nan)); // UNKNOWN

    //__buitin_isinf_sign(x):
    assert(__builtin_isinf_sign(1.0));  // FAIL
    assert(__builtin_isinf_sign(inf));  // UNKNOWN
    assert(__builtin_isinf_sign(-inf)); // UNKNOWN
    assert(__builtin_isinf_sign(nan));  // UNKNOWN

    //__buitin_isnan(x):
    assert(__builtin_isnan(1.0)); // FAIL
    assert(__builtin_isnan(inf)); // UNKNOWN
    assert(__builtin_isnan(nan)); // UNKNOWN

    //__buitin_isnormal(x):
    assert(__builtin_isnormal(dbl_min));     // SUCCESS
    assert(__builtin_isnormal(0.0));         // FAIL
    assert(__builtin_isnormal(dbl_min / 2)); // FAIL
    assert(__builtin_isnormal(inf));         // UNKNOWN
    assert(__builtin_isnormal(nan));         // UNKNOWN

    //__buitin_signbit(x):
    assert(__builtin_signbit(1.0));  // FAIL
    assert(__builtin_signbit(-1.0)); // SUCCESS
    assert(__builtin_signbit(0.0));  // UNKNOWN
    assert(__builtin_signbit(inf));  // UNKNOWN
    assert(__builtin_signbit(-inf)); // UNKNOWN
    assert(__builtin_signbit(nan));  // UNKNOWN

    // fabs(x):
    assert(4. == fabs(-4.));         // SUCCESS
    assert(0. <= fabs(cos(0.1)));    // SUCCESS
    assert(0. <= fabs(-inf));        // UNKNOWN
    assert(0. <= fabs(nan));         // UNKNOWN

    double greater_than_pi = 3.142;
    // acos(x):
    assert((0. <= acos(0.1)) && (acos(0.1) <= greater_than_pi)); // SUCCESS
    assert(acos(1.) == 0.);                                      // SUCCESS
    acos(2.0);                                                   // WARN: Domain error might occur: acos argument might be outside of [-1., 1.]

    // asin(x):
    assert(((-greater_than_pi / 2.) <= asin(0.1)) && (asin(0.1) <= (greater_than_pi / 2.))); // SUCCESS
    assert(asin(0.) == 0.);                                                                  // SUCCESS
    asin(2.0);                                                                               // WARN: Domain error might occur: asin argument might be outside of [-1., 1.]

    // atan(x):
    assert(((-greater_than_pi / 2.) <= atan(0.1)) && (atan(0.1) <= (greater_than_pi / 2.))); // SUCCESS
    assert(atan(0.) == 0.);                                                                  // SUCCESS

    // atan2(y, x)
    assert(((-greater_than_pi / 2.) <= atan2(0.1, 0.2)) && (atan2(0.1, 0.2) <= (greater_than_pi / 2.))); // SUCCESS

    // cos(x)
    assert((-1. <= cos(0.1)) && (cos(0.1) <= 1.)); // SUCCESS
    assert(cos(0.) == 1.);                         // SUCCESS

    // sin(x)
    assert((-1. <= sin(0.1)) && (sin(0.1) <= 1.)); // SUCCESS
    assert(sin(0.) == 0.);                         // SUCCESS

    // tan(x)
    assert(tan(0.) == 0.); // SUCCESS

    // unimplemented math.h function, should not invalidate globals:
    j0(0.1);       // NOWARN
    ldexp(0.1, 1); // NOWARN
}
