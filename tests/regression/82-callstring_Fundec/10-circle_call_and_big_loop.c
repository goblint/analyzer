// PARAM: --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set
// Checks proper handling of recursions in loops + shows that not all 200 iterations are analyzed
#include <stdio.h>

int num_iterat = 2;

int f(int i);

int g(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 1;
    }
    if (i > 0)
    {
        res = f(--i);
    }
    return res;
}

int f(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 2;
    }
    if (i > 0)
    {
        res = g(--i);
    }
    return res;
}

int main(void)
{
    for (int i = 200; i > 0; i--)
    {
        int res1 = f(num_iterat);
        int res2 = g(num_iterat);
        __goblint_check(res1 == 2);
        __goblint_check(res2 == 1);
    }
}