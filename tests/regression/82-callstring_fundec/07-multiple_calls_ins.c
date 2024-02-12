// PARAM: --set ana.context.callStack_height 5 --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set
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
    // main -> f(7) -> ... -> f(0) -> return 11
    // [main, f, f, f, f] and [f, f, f, f, f] (4 times)
    __goblint_check(f(7) == 11); // UNKNOWN

    // main -> g(20) -> f(19) -> ... -> f(0) -> return 11
    // [main, g, f, f, f] and [g, f, f, f, f] and [f, f, f, f, f] (16 times)
    __goblint_check(g(20) == 11); // UNKNOWN

    // main -> h(10) -> g(9) -> f(8) -> ... -> f(1) -> f(0) -> return 11
    // [main, h, g, f, f] and [h, g, f, f, f] and [g, f, f, f, f] and [f, f, f, f, f] (5 times)
    __goblint_check(h(10) == 11); // UNKNOWN
}
