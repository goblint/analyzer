// PARAM: --enable ana.int.interval_set --enable ana.context.ctx_gas --set ana.context.ctx_gas_value 10
// Tests multiple recursive function calls
#include <stdio.h>

int h(int i)
{
    if (i == 0)
    {
        return 3;
    }
    if (i > 0)
    {
        return h(i - 1);
    }
}

int g(int i)
{
    if (i == 0)
    {
        return 2;
    }
    if (i > 0)
    {
        return h(i - 1);
    }
}

int f(int i)
{
    if (i == 0)
    {
        return 1;
    }
    if (i > 0)
    {
        return g(i - 1);
    }
}

int main(void)
{
    __goblint_check(f(10) == 3);
    __goblint_check(f(8) == 3);
    __goblint_check(h(5) == 3);

    __goblint_check(f(0) == 1);
    __goblint_check(f(1) == 2);
    __goblint_check(g(0) == 2);
    __goblint_check(g(1) == 3);
}
