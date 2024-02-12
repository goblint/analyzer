// PARAM: --set ana.context.callStack_height 10 --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set
// Basic example
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

int g(int i)
{
    if (i == 0)
    {
        return 12;
    }
    if (i > 0)
    {
        return f(i - 1);
    }
    return 2;
}

int h(int i)
{
    if (i == 0)
    {
        return 13;
    }
    if (i > 0)
    {
        return g(i - 1);
    }
    return 3;
}

int main(void)
{
    // main -> f(0) -> return 11
    // [main, f]
    __goblint_check(f(0) == 11);

    // main -> g(0) -> return 12
    // [main, g]
    __goblint_check(g(0) == 12);

    // main -> h(0) -> return 13
    // [main, h]
    __goblint_check(h(0) == 13);
}
