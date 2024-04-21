// PARAM: --enable ana.int.interval_set --set ana.context.gas_value 0 --enable solvers.td3.narrow-sides.enabled  --enable solvers.td3.narrow-sides.stable
// Taken from context gas tests, where the assertions were unknown.
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
    return 13;
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
    return 12;
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
    return 11;
}

int main(void)
{
    __goblint_check(f(11) == 3);
    __goblint_check(g(12) == 3);
    __goblint_check(g(20) == 3);
    __goblint_check(f(20) == 3);
    __goblint_check(h(40) == 3);
    __goblint_check(h(300) == 3);
    __goblint_check(f(300) == 3);
}
