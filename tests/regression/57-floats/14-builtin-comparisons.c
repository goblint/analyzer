// PARAM: --enable ana.float.interval
#include <math.h>
#include <goblint.h>

int g = 8;

int main(void)
{
    int x = isgreater(2.0, 1.0);
    __goblint_check(x);
    x = !isgreater(1.0, 2.0);
    __goblint_check(x);
    x = isgreater(INFINITY, 1.0);
    __goblint_check(x);
    x = !isgreater(1.0, NAN);
    __goblint_check(x);
    x = !isgreater(1.0, INFINITY);
    __goblint_check(x);
    x = !isgreater(INFINITY, INFINITY);
    __goblint_check(x);

    __goblint_check(isgreaterequal(2.0, 1.0));
    __goblint_check(!isgreaterequal(1.0, 2.0));
    __goblint_check(isgreaterequal(1.0, 1.0));
    __goblint_check(isgreaterequal(INFINITY, 1.0));
    __goblint_check(!isgreaterequal(1.0, NAN));
    __goblint_check(isgreaterequal(INFINITY, INFINITY));

    // Check globals have not been invalidated
    __goblint_check(g == 8);
    return 0;
}
