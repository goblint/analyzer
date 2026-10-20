// PARAM: --enable ana.float.interval --enable ana.int.interval
// Converting doubles beyond 2^53 to integers must keep all mantissa bits.
#include <goblint.h>

int main()
{
    double d = 9007199254740994.0; // 2^53 + 2
    long long x = (long long)d;
    __goblint_check(x == 9007199254740994LL);

    double e = -9007199254740994.0;
    long long y = (long long)e;
    __goblint_check(y == -9007199254740994LL);

    double f = 18014398509481988.0; // 2^54 + 4
    long long z = (long long)f;
    __goblint_check(z == 18014398509481988LL);

    return 0;
}
