// PARAM: --enable ana.int.interval_set --set exp.unrolling-factor 3 --set ana.context.gas_value 10
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
    int res1 = 0;
    int res2 = 0;

    // context sensitive analysis
    for (int i = 5; i > 0; i--)
    {
        res1 = f(3);
        res2 = f(i);
        __goblint_check(res1 == 11);
        __goblint_check(res2 == 11);
    }
    __goblint_check(res1 == 11);
    __goblint_check(res2 == 11);
}
