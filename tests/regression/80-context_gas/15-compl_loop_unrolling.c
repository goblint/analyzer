// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set --set exp.unrolling-factor 3 --set ana.context.ctx_gas_value 10
// TODO
#include <stdio.h>

int f(int i)
{
    if (i == 0)
    {
        return 11;
    }
    if (i > 0)
    {
        return f(i - 1);
    }
    return 1;
}

int main(void)
{
    // loop should be completely unrolled
    for (int i = 3; i > 0; i--)
    {
        __goblint_check(f(1) == 11);
        __goblint_check(f(20) == 11); // UNKNOWN
    }

    for (int i = 1; i > 0; i--)
    {
        __goblint_check(f(2) == 11);
        __goblint_check(f(2000) == 11); // UNKNOWN
    }
}
