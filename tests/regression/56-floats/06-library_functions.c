// PARAM: --enable ana.float.interval --enable ana.int.interval
#include <assert.h>
#include <math.h>
#include <float.h>

int main() {
    double dbl_min = 2.2250738585072014e-308;
    double inf = 1. / 0.;
    double nan = 0. / 0.;

    //__buitin_isfinite(x):
    assert(__builtin_isfinite(1.0)); //SUCCESS!
    assert(__builtin_isfinite(inf)); //UNKNOWN
    assert(__builtin_isfinite(nan)); //UNKNOWN

    //__buitin_isinf(x):
    assert(__builtin_isinf(1.0)); //FAIL!
    assert(__builtin_isinf(inf)); //UNKNOWN
    assert(__builtin_isinf(nan)); //UNKNOWN

    //__buitin_isinf_sign(x):
    assert(__builtin_isinf_sign(1.0)); //FAIL!
    assert(__builtin_isinf_sign(inf)); //UNKNOWN
    assert(__builtin_isinf_sign(- inf)); //UNKNOWN
    assert(__builtin_isinf_sign(nan)); //UNKNOWN

    //__buitin_isnan(x):
    assert(__builtin_isnan(1.0)); //FAIL!
    assert(__builtin_isnan(inf)); //UNKNOWN
    assert(__builtin_isnan(nan)); //UNKNOWN

    //__buitin_isnormal(x):
    assert(__builtin_isnormal(dbl_min)); //SUCCESS!
    assert(__builtin_isnormal(0.0)); //FAIL!
    assert(__builtin_isnormal(dbl_min / 2)); //FAIL!
    assert(__builtin_isnormal(inf)); //UNKNOWN
    assert(__builtin_isnormal(nan)); //UNKNOWN

    //__buitin_signbit(x):
    assert(__builtin_signbit(1.0)); //FAIL!
    assert(__builtin_signbit(-1.0)); //SUCCESS!
    assert(__builtin_signbit(0.0)); //UNKNOWN
    assert(__builtin_signbit(inf)); //UNKNOWN
    assert(__builtin_signbit(- inf)); //UNKNOWN
    assert(__builtin_signbit(nan)); //UNKNOWN

    //unimplemented math.h function, should not invalidate globals:
    cos(0.1); //NOWARN
    j0(0.1); //NOWARN
    ldexp(0.1, 1); //NOWARN
}
