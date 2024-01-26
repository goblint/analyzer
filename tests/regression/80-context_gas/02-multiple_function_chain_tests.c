// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set --set ana.context.ctx_gas_value 10
// Tests multiple recursive function calls
#include <stdio.h>

int h(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 1;
    }
    if (i > 0)
    {
        res = h(--i);
    }
    return res;
}

int g(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 2;
    }
    if (i > 0)
    {
        res = h(--i);
    }
    return res;
}

int f(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 3;
    }
    if (i > 0)
    {
        res = g(--i);
    }
    return res;
}

int main(void)
{
    __goblint_check(f(-5) == 0);
    __goblint_check(f(0) == 3);
    __goblint_check(f(1) == 2);
    __goblint_check(f(2) == 1);
    __goblint_check(f(7) == 1);  // boundary (included)
    __goblint_check(f(8) == 1);  // UNKNOWN  //boundary (excluded)
    __goblint_check(f(20) == 1); // UNKNOWN

    __goblint_check(g(-10) == 0);
    __goblint_check(g(0) == 2);
    __goblint_check(g(1) == 1);

    __goblint_check(h(-10) == 0);
    __goblint_check(h(0) == 1);
    __goblint_check(h(7) == 1); // boundary (included)
    __goblint_check(h(8) == 1); // UNKNOWN //boundary (excluded)

    int res1 = h(5);
    int res2 = h(4);
    int result = res1 + res2;
    __goblint_check(result == 2);

    __goblint_check(f(g(h(7))) == 2); // h(7) = 1; g(1) = 1; f(1) = 2;
    __goblint_check(f(g(h(8))) == 2); // UNKNOWN  // h(8) = UNKNOWN
}
