// PARAM: --set ana.context.callStack_height 2 --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set
// Basic example
#include <stdio.h>

int f(int i)
{
    if (i == 0)
    {
        return 1;
    }
    if (i > 0)
    {
        return f(i - 1);
    }
    return 0;
}

int g(int i)
{
    if (i == 2)
    {
        return f(i - 1) + 1;
    }
    if (i > 0)
    {
        return g(i - 1) + 0;
    }
    return 1;
}

int main(void)
{
    // main -> g(6) -> ... -> g(2) -> f(1) -> f(0) -> return 1
    // [main, g] and [g, g] (3 times) and [g, f] and [f, f]
    __goblint_check(g(5) == 2);
}
