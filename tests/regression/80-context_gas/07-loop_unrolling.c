// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set --set exp.unrolling-factor 3 --set ana.context.ctx_gas_value 10
// TODO
#include <stdio.h>

int f(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 1;
    }
    if (i > 0)
    {
        res = f(--i);
    }
    return res;
}

int main(void)
{
    for (int i = 1; i > 0; i--)
    {
        __goblint_check(f(2) == 1);
        __goblint_check(f(2000) == 1); // UNKNOWN
    }

    // loop should be completely unrolled
    for (int i = 3; i > 0; i--)
    {
        __goblint_check(f(1) == 1);
        __goblint_check(f(20) == 1); // UNKNOWN
    }

    int res1 = 0;
    int res2 = 0;
    int result = 0;

    // context sensitive analysis
    for (int i = 5; i > 0; i--)
    {
        res1 = f(3);
        res2 = f(i);
        __goblint_check(res1 == 1);
        __goblint_check(res2 == 1); // TODO
        result += res1 + res2;
    }
    __goblint_check(res1 == 1);
    __goblint_check(res2 == 1);    // TODO
    __goblint_check(result == 10); // TODO

    for (int i = 5; i > 0; i--)
    {
        __goblint_check(f(0) == 1);
        __goblint_check(f(7) == 1); // boundary (included))
        __goblint_check(f(8) == 1); // UNKNOWN //boundary (excluded)
    }

    // high number of iterations
    for (int i = 500; i > 0; i--)
    {
        __goblint_check(f(i) == 1); // UNKNOWN
        __goblint_check(f(4) == 1);
    }
}
