// PARAM: --set "ana.activated[+]" loopfree_callstring --enable ana.int.interval_set
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
    if (i == 5)
    {
        return f(i - 1);
    }
    if (i > 0)
    {
        return g(i - 1);
    }
    return 1;
}

int main(void)
{
    // main -> g(20) -> ... -> g(5) -> f(4) -> ... -> f(0) -> return 1
    // [main] {g, f}
    __goblint_check(g(20) == 1); // UNKNOWN
}
