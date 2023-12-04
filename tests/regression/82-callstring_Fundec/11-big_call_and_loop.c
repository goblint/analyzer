// PARAM: --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set
// Checks proper handling of recursions in loops + should show that analyzing the same function twice with different number of recursions fastens the result
// TODO weird behavior: only if num_iterat = 9 (= same value as for f(...) and g(...))
#include <stdio.h>

int num_iterat = 10;

int f(int i)
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

int g(int i)
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
    int res1 = f(9);
    int res2 = g(9);
    __goblint_check(res1 == 1); // TODO
    __goblint_check(res2 == 2); // TODO
    for (int i = 10; i > 0; i--)
    {
        int res3 = f(num_iterat);
        int res4 = g(num_iterat);
        __goblint_check(res3 == 1); // TODO
        __goblint_check(res4 == 2); // TODO
    }
}
