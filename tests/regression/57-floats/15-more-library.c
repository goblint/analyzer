// PARAM: --enable ana.float.interval
#include <math.h>
#include <goblint.h>

int g = 8;

int main(void)
{
    __goblint_check(fabs(+3.0) == +3.0);
    __goblint_check(fabs(-3.0) == +3.0);
    __goblint_check(fabs(-0.0) == -0.0);
    __goblint_check(fabs(-0.0) == +0.0);
    __goblint_check(fabs(-INFINITY) == INFINITY);
    __goblint_check(isnan(fabs(-NAN)));

    // Check globals have not been invalidated
    __goblint_check(g == 8);
    return 0;
}
