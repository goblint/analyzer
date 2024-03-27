// PARAM: --set "ana.activated[+]" loopfree_callstring --set ana.ctx_sens "['loopfree_callstring']"  --enable ana.int.interval_set
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
    // main -> f(4) -> f(3) -> f(2) -> f(1) -> f(0) -> return 11
    // [main, f] and [main, {f}] (4 times)
    __goblint_check(f(4) == 11); // UNKNOWN

    // main -> g(20) -> f(19) -> ... -> f(0) -> return 11
    // [main, g, f] and [main, g, {f}] (20 times)
    __goblint_check(g(20) == 11); // UNKNOWN

    // main -> h(10) -> g(9) -> f(8) -> ... -> f(1) -> f(0) -> return 11
    // [main, h, g, f] and [main, h, g, {f}] (9 times)
    __goblint_check(h(10) == 11); // UNKNOWN
}
