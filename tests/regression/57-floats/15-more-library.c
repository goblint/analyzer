// PARAM: --enable ana.float.interval
#include <math.h>
#include <goblint.h>

int g = 8;

int main(void)
{
    int r = __isnan(3.0);
    r = __isnanf(3.0f);
    r = __isnanl(3.0l);
    r = __signbit(3.0);
    r = __signbitf(3.0f);
    r = __signbitl(3.0l);
    r = __isinf(3.0);
    r = __isinff(3.0f);
    r = __isinfl(3.0l);
    r = __finite(3.0);
    r = __finitef(3.0f);
    r = __finitel(3.0l);

    __goblint_check(fabs(+3.0) == +3.0);
    __goblint_check(fabs(-3.0) == +3.0);
    __goblint_check(fabs(-0.0) == -0.0);
    __goblint_check(fabs(-0.0) == +0.0);
    __goblint_check(fabs(-INFINITY) == INFINITY);
    __goblint_check(isnan(fabs(-NAN)));

    __goblint_check(ceil(2.4) == 3.0);
    __goblint_check(ceil(-2.4) == -2.0);

    double c = ceil(-0.0);
    __goblint_check((c == -0.0) && signbit(c)); //TODO (We do not distinguish +0 and -0)

    c = ceil(-INFINITY);

    // On OS X this gets expanded differently than on Linux where it is equivalent to the one below
    // Might make sense to check what is needed for OS X support in the future, but this is not a deal-breaker
    // and not high priority for now.
    // __goblint_check(isinf(INFINITY) && signbit(c));

    __goblint_check(isinf(INFINITY) && __builtin_signbit(c));

    __goblint_check(floor(2.7) == 2.0);
    __goblint_check(floor(-2.7) == -3.0);

    double c = floor(-0.0);
    __goblint_check((c == -0.0) && signbit(c)); //TODO (We do not distinguish +0 and -0)

    c = floor(-INFINITY);

    // On OS X this gets expanded differently than on Linux where it is equivalent to the one below
    // Might make sense to check what is needed for OS X support in the future, but this is not a deal-breaker
    // and not high priority for now.
    // __goblint_check(isinf(INFINITY) && signbit(c));

    __goblint_check(isinf(INFINITY) && __builtin_signbit(c));

    // Check globals have not been invalidated
    __goblint_check(g == 8);
    return 0;
}
