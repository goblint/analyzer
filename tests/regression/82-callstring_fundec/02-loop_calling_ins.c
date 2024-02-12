// PARAM: --set ana.context.callStack_height 10 --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set
// Basic example
#include <stdio.h>

int f(int i);

int g(int i)
{
    if (i > 0)
    {
        return f(i - 1);
    }
    return 2;
}

int f(int i)
{
    if (i > 0)
    {
        return g(i - 1);
    }
    return 1;
}

int main(void)
{
    // main -> f(15) -> g(14) -> f(13) -> ... -> g(2) -> f(1) -> g(0) -> return 2
    // [main, f, g, f, g, f, g, f, g, f] and
    // [f, g, f, g, f, g, f, g, f, g] (4 times) and
    // [g, f, g, f, g, f, g, f, g, f] (3 times)
    __goblint_check(f(15) == 2); // UNKNOWN
}
