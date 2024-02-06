// PARAM: --set "ana.activated[+]" loopfreeCallstring --enable ana.int.interval_set
// Basic example
#include <stdio.h>

int num_iterat = 10;
int a = 20;

int f(int i);

int g(int i)
{
    if (i > 0)
    {
        a = --i;
        g(i);
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
    f(num_iterat);
    __goblint_check(a == 0); // UNKNOWN
}
