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
    // main -> f(3) -> ... -> f(0) -> return 11
    // [main, f, f, f, f]
    __goblint_check(f(3) == 11);

    // main -> f(6) -> ... -> f(0) -> return 11
    // [main, f, f, f, f] and [f, f, f, f, f] (3 times)
    __goblint_check(f(6) == 11);

    // main -> g(3) -> f(2) -> f(1) -> f(0) -> return 11
    // [main, g, f, f, f]
    __goblint_check(g(3) == 11);

    // main -> h(3) -> g(2) -> f(1) -> f(0) -> return 11
    // [main, h, g, f, f]
    __goblint_check(h(3) == 11);
}
