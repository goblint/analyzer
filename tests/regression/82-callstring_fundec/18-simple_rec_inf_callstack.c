// PARAM: --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set --enable ana.context.inf_callStack
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
    f(25);
    __goblint_check(a == 0);
}
