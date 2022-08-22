// PARAM: --enable ana.float.interval --enable ana.int.interval
#include <assert.h>
#include <math.h>
#include <float.h>

int main()
{
    double dbl_min = 2.2250738585072014e-308;
    double inf = __builtin_inf();
    double nan = __builtin_nan("");

    //__buitin_isfinite(x):
    __goblint_check(__builtin_isfinite(1.0)); // SUCCESS
    __goblint_check(__builtin_isfinite(inf)); // UNKNOWN
    __goblint_check(__builtin_isfinite(nan)); // UNKNOWN

    //__buitin_isinf(x):
    __goblint_check(__builtin_isinf(1.0)); // FAIL
    __goblint_check(__builtin_isinf(inf)); // UNKNOWN
    __goblint_check(__builtin_isinf(nan)); // UNKNOWN

    //__buitin_isinf_sign(x):
    __goblint_check(__builtin_isinf_sign(1.0));  // FAIL
    __goblint_check(__builtin_isinf_sign(inf));  // UNKNOWN
    __goblint_check(__builtin_isinf_sign(-inf)); // UNKNOWN
    __goblint_check(__builtin_isinf_sign(nan));  // UNKNOWN

    //__buitin_isnan(x):
    __goblint_check(__builtin_isnan(1.0)); // FAIL
    __goblint_check(__builtin_isnan(inf)); // UNKNOWN
    __goblint_check(__builtin_isnan(nan)); // UNKNOWN

    //__buitin_isnormal(x):
    __goblint_check(__builtin_isnormal(dbl_min));     // SUCCESS
    __goblint_check(__builtin_isnormal(0.0));         // FAIL
    __goblint_check(__builtin_isnormal(dbl_min / 2)); // FAIL
    __goblint_check(__builtin_isnormal(inf));         // UNKNOWN
    __goblint_check(__builtin_isnormal(nan));         // UNKNOWN

    //__buitin_signbit(x):
    __goblint_check(__builtin_signbit(1.0));  // FAIL
    __goblint_check(__builtin_signbit(-1.0)); // SUCCESS
    __goblint_check(__builtin_signbit(0.0));  // UNKNOWN
    __goblint_check(__builtin_signbit(inf));  // UNKNOWN
    __goblint_check(__builtin_signbit(-inf)); // UNKNOWN
    __goblint_check(__builtin_signbit(nan));  // UNKNOWN

    // fabs(x):
    __goblint_check(4. == fabs(-4.));         // SUCCESS
    __goblint_check(0. <= fabs(cos(0.1)));    // SUCCESS
    __goblint_check(0. <= fabs(-inf));        // UNKNOWN
    __goblint_check(0. <= fabs(nan));         // UNKNOWN

    double greater_than_pi = 3.142;
    // acos(x):
    __goblint_check((0. <= acos(0.1)) && (acos(0.1) <= greater_than_pi)); // SUCCESS
    __goblint_check(acos(1.) == 0.);                                      // SUCCESS
    acos(2.0);                                                   // WARN: Domain error might occur: acos argument might be outside of [-1., 1.]

    // asin(x):
    __goblint_check(((-greater_than_pi / 2.) <= asin(0.1)) && (asin(0.1) <= (greater_than_pi / 2.))); // SUCCESS
    __goblint_check(asin(0.) == 0.);                                                                  // SUCCESS
    asin(2.0);                                                                               // WARN: Domain error might occur: asin argument might be outside of [-1., 1.]

    // atan(x):
    __goblint_check(((-greater_than_pi / 2.) <= atan(0.1)) && (atan(0.1) <= (greater_than_pi / 2.))); // SUCCESS
    __goblint_check(atan(0.) == 0.);                                                                  // SUCCESS

    // atan2(y, x)
    __goblint_check(((-greater_than_pi / 2.) <= atan2(0.1, 0.2)) && (atan2(0.1, 0.2) <= (greater_than_pi / 2.))); // SUCCESS

    // cos(x)
    __goblint_check((-1. <= cos(0.1)) && (cos(0.1) <= 1.)); // SUCCESS
    __goblint_check(cos(0.) == 1.);                         // SUCCESS

    // sin(x)
    __goblint_check((-1. <= sin(0.1)) && (sin(0.1) <= 1.)); // SUCCESS
    __goblint_check(sin(0.) == 0.);                         // SUCCESS

    // tan(x)
    __goblint_check(tan(0.) == 0.); // SUCCESS

    // unimplemented math.h function, should not invalidate globals:
    j0(0.1);       // NOWARN
    ldexp(0.1, 1); // NOWARN
}
