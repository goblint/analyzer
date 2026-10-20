// PARAM: --enable ana.float.interval --enable ana.float.evaluate_math_functions --enable ana.int.interval --set ana.activated[+] tmpSpecial
#include <math.h>
#include <stdint.h>
#include <inttypes.h>
#include <goblint.h>

int main()
{
    int i;
    long long x;
    int64_t data;

    if ((double)i <= 5.0) {
        __goblint_check(i <= 5);
    }
    if ((double)i < 5.0) {
        __goblint_check(i <= 4);
    }
    if ((double)i > 4.5) {
        __goblint_check(i >= 5);
    }
    if ((long double)i >= -2.5) {
        __goblint_check(i >= -2);
    }

    // 2^53 + 1 is converted to 2^53 under round-to-nearest
    if ((double)x <= 9007199254740992.0) {
        __goblint_check(x <= 9007199254740993LL);
        __goblint_check(x <= 9007199254740992LL); //UNKNOWN!
    }

    // 2^25 + 1 is converted to 2^25 under round-to-nearest, the next float after 2^25 is 2^25 + 4
    if (i >= 0 && (float)i <= 33554432.0f) {
        __goblint_check(i <= 33554435);
        __goblint_check(i <= 33554432); //UNKNOWN!
    }
    if ((float)i < 5.0f) {
        __goblint_check(i <= 4);
    }

    // pattern from SV-COMP Juliet CWE190 int64_t square tasks
    if (data > INT64_MIN && imaxabs((intmax_t)data) <= sqrtl(0x7fffffffffffffffLL)) {
        __goblint_check(data <= 3037000499LL);
        __goblint_check(data >= -3037000499LL);
        int64_t result = data * data; // NOWARN
    }

    // non-constant bound
    int k;
    if (k >= 2 && k <= 7) {
        double d = k + 0.5; // d in [2.5, 7.5]
        if ((double)i <= d) {
            __goblint_check(i <= 7);
            __goblint_check(i <= 6); //UNKNOWN!
        }
        if ((double)i > d) {
            __goblint_check(i >= 3);
            __goblint_check(i >= 4); //UNKNOWN!
        }
    }

    // no integer is converted to 4.5
    if ((double)i == 4.5) {
        __goblint_check(0); // NOWARN
    }

    // infinite bounds
    if ((double)i <= INFINITY) {
        __goblint_check(i <= 7); //UNKNOWN!
    }
    if ((double)i >= -INFINITY) {
        __goblint_check(i >= -7); //UNKNOWN!
    }
    if ((double)i > INFINITY) {
        __goblint_check(0); // NOWARN
    }
    int flag;
    double up = 2.5;
    if (flag) {
        up = INFINITY; // up in [2.5, inf]
    }
    if ((double)i <= up) {
        __goblint_check(i <= 2); //UNKNOWN!
    }
    if ((double)i >= up) {
        __goblint_check(i >= 3); // TODO (float interval with infinite bound is top)
    }

    // NaN, every comparison except != is false
    if ((double)i <= NAN) {
        __goblint_check(0); // NOWARN
    }
    if ((double)i == NAN) {
        __goblint_check(0); // NOWARN
    }
    double maybe_nan = 7.5;
    if (flag) {
        maybe_nan = NAN;
    }
    if ((double)i <= maybe_nan) {
        __goblint_check(i <= 7); // TODO (float interval with NaN is top)
    }

    return 0;
}
