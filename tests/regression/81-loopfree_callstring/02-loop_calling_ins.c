// PARAM: --set "ana.activated[+]" loopfree_callstring --enable ana.int.interval_set
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
    // main -> f(9) -> g(8) -> f(7) -> ... -> g(2) -> f(1) -> g(0) -> return 2
    // [main, f, g] and [main, {f, g}] (4 times)
    __goblint_check(f(9) == 2); // UNKNOWN
}
