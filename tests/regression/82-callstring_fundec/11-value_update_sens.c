// PARAM: --set ana.context.callStack_height 10 --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set
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
    // [main, f, f, f, f, f, f, f, f, f] and [f, f, f, f, f, f, f, f, f, f]
    f(9);
    __goblint_check(a == 0);
}
