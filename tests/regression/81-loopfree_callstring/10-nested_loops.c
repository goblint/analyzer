// PARAM: --set "ana.activated[+]" loopfree_callstring --set ana.ctx_sens "['loopfree_callstring']"  --enable ana.int.interval_set
// Basic example
#include <stdio.h>

int h(int i)
{
    return 3;
}

int g(int i)
{
    return h(i);
}

int f(int i)
{
    if (i == 0)
    {
        return g(2);
    }
    if (i > 0)
    {
        return f(i - 1);
    }
}

int main(void)
{
    // main -> f(4) -> f(3) -> ... -> f(0) -> g(2) -> h(2) -> return 3
    // [main, {f}, g, h] (4 times)
    __goblint_check(f(4) == 3); // UNKNOWN
}
