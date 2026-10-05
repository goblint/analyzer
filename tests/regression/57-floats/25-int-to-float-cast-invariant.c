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

    // 2^25 + 1 is converted to 2^25 under round-to-nearest, conversion to float must not be refined using double precision
    if (i >= 0 && (float)i <= 33554432.0f) {
        __goblint_check(i <= 33554432); //UNKNOWN!
    }

    // pattern from SV-COMP Juliet CWE190 int64_t square tasks
    if (data > INT64_MIN && imaxabs((intmax_t)data) <= sqrtl(0x7fffffffffffffffLL)) {
        __goblint_check(data <= 3037000499LL);
        __goblint_check(data >= -3037000499LL);
        int64_t result = data * data; // NOWARN
    }

    return 0;
}
