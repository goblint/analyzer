// PARAM: --set "ana.activated[+]" loopfreeCallstring --enable ana.int.interval_set
// Basic example
#include <stdio.h>

int num_iterat = 3;

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
        res = 1;
    }
    if (i > 0)
    {
        res = f(--i);
    }
    return res;
}

int h(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 1;
    }
    if (i > 0)
    {
        res = g(--i);
    }
    return res;
}

int main(void)
{
    int res1 = f(num_iterat);
    __goblint_check(res1 == 1); // UNKNOWN
    int res2 = f(num_iterat);
    __goblint_check(res2 == 1); // UNKNOWN
    // int res3 = g(num_iterat);
    int res4 = h(num_iterat);
    __goblint_check(res4 == 1);

    // int result = res1 + res2 + res3 + res4 + res5;
    //__goblint_check(result == 5);
}
