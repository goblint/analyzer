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
    // high number of iterations
    for (int i = 500; i > 0; i--)
    {
        __goblint_check(f(i) == 11); // UNKNOWN
        __goblint_check(f(4) == 11);
    }
}
