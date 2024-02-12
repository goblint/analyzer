// PARAM: --set ana.context.callStack_height 10 --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set
// Checks proper handling of recursions in loops + shows that not all 90 recursions are analyzed
#include <stdio.h>

int f(int i);

int g(int i)
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

int f(int i)
{
    if (i == 0)
    {
        return 2;
    }
    if (i > 0)
    {
        return g(i - 1);
    }
    return 12;
}

int main(void)
{
    int res1 = 0;
    int res2 = 0;
    for (int i = 2; i > 0; i--)
    {
        res1 = f(200);
        res2 = g(200);
        __goblint_check(res1 == 2); // UNKNOWN
        __goblint_check(res2 == 1); // UNKNOWN
    }
}
