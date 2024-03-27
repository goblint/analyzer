// PARAM: --set "ana.activated[+]" loopfree_callstring --set ana.ctx_sens "['loopfree_callstring']"   --enable ana.int.interval_set
// Basic example
#include <stdio.h>

int a = 20;

int f(int i)
{
    if (i > 0)
    {
        a = --i;
        f(i);
    }
    return 0;
}

int main(void)
{
    // main -> f(10) -> f(9) -> ... f(0)
    // [main, f] and [main, {f}] (2 times)
    f(2);
    __goblint_check(a == 0); // UNKNOWN
}
