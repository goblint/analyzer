// PARAM: --set ana.context.callStack_height 2 --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set --set exp.unrolling-factor 3

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
    return 11;
}

int main(void)
{
    for (int i = 5; i > 0; i--)
    {
        // main -> f(3) -> ... -> f(0) -> return 1
        // [main, f] and [f, f] (3 times)
        __goblint_check(f(3) == 1);
    }
}
