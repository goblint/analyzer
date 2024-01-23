// PARAM: --set "ana.activated[+]" callstringTracking --enable ana.int.interval_set
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
    if (i == 2)
    {
        res = f(--i);
    }
    if (i > 0)
    {
        res = g(--i);
    }
    return res;
}


int main(void)
{
    int res1 = g(num_iterat);
    __goblint_check(res1 == 1); //UNKNOWN
}
