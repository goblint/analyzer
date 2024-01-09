// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set --set ana.context.ctx_gas_value 15
// Checks if recursion in loops is handled properly
#include <stdio.h>

int f(int i);

int g(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 1;
    }
    if (i > 0)
    {
        res = f(i + 1);
    }
    return res;
}

int f(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 2;
    }
    if (i > 0)
    {
        res = g(i - 2) + g(i - 3);
    }
    return res;
}

int main(void)
{
    printf("%i, %i\n", f(200), g(10));

    __goblint_check(f(0) == 2);
    __goblint_check(f(3) == 2);
    __goblint_check(f(7) == 13);
    __goblint_check(f(8) == 21);    // UNKNOWN //boundary (excluded)
    __goblint_check(f(9) == 34);    // UNKNOWN
    __goblint_check(f(20) == 6765); // UNKNOWN

    __goblint_check(g(3) == 3);
    __goblint_check(g(6) == 13);     // boundary (included)
    __goblint_check(g(7) == 21);     // UNKNOWN
    __goblint_check(g(8) == 34);     // UNKNOWN
    __goblint_check(g(20) == 10946); // UNKNOWN
}
