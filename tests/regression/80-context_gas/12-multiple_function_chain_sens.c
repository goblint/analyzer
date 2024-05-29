// PARAM: --enable ana.int.interval_set --set ana.context.gas_value 10
// Note: 11 function calls are analyzed context-sensitively
// -> tracked parameter in domain enables one additional context-sensitively analyzed call
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
    __goblint_check(f(10) == 3);
    __goblint_check(f(8) == 3);
    __goblint_check(h(5) == 3);

    __goblint_check(f(0) == 1);
    __goblint_check(f(1) == 2);
    __goblint_check(g(0) == 2);
    __goblint_check(g(1) == 3);
}
