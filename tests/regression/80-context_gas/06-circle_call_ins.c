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

    __goblint_check(f(13) == 233);  // UNKNOWN
    __goblint_check(f(20) == 6765); // UNKNOWN

    __goblint_check(g(13) == 377);   // UNKNOWN
    __goblint_check(g(20) == 10946); // UNKNOWN
}
