// PARAM: --set "ana.activated[+]" loopfree_callstring --set ana.ctx_sens "['loopfree_callstring']" --enable ana.int.interval_set
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
    // [main, f] and [main, {f}] (3 times)
    __goblint_check(f(3) == 11);

    // main -> g(3) -> f(2) -> f(1) -> f(0) -> return 11
    // [main, g, f] and [main, g, {f}]
    __goblint_check(g(3) == 11);

    // main -> h(5) -> g(4) -> f(3) -> ... -> f(0) -> return 11
    // [main, h, g, f] and [main, h, g, {f}]
    __goblint_check(h(5) == 11);
}
