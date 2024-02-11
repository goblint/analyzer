// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set --set ana.context.ctx_gas_value 15
// Checks if recursion in loops is handled properly
#include <stdio.h>

int f(int i);

int g(int i)
{
    if (i == 0)
    {
        return 1;
    }
    if (i > 0)
    {
        return f(i + 1);
    }
    return 11;
}

int f(int i)
{
    if (i == 0)
    {
        return 2;
    }
    if (i > 0)
    {
        return g(i - 2) + g(i - 3);
    }
    return 12;
}

int main(void)
{
    __goblint_check(f(0) == 2);
    __goblint_check(f(7) == 101);
    __goblint_check(f(9) == 265);
    __goblint_check(f(10) == 429);

    __goblint_check(g(0) == 1);
    __goblint_check(g(3) == 25);
    __goblint_check(g(6) == 101);
    __goblint_check(g(8) == 265);
}
