// PARAM: --set "ana.activated[+]" callstringTracking --enable ana.int.interval_set
// Basic example
#include <stdio.h>

int a;

int f(int i);

int g(int i)
{
    if (i > 0)
    {
        a = --i;
        f(i);
    }
    return 0;
}

int f(int i)
{
    if (i > 0)
    {
        a = --i;
        g(i);
    }
    return 0;
}

int main(void)
{
    a = 20;
    f(3);
    __goblint_check(a == 0);
}
