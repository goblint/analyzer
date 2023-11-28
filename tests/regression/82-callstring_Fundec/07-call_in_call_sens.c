// PARAM: --enable ana.context.callstring_fundec --enable ana.int.interval_set
// Interesting if nested recursions are handled properly
#include <stdio.h>

int num_iterat = 9; 

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

int h(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 3;
    }
    if (i > 0)
    {
        res = h(--i);
    }
    return res;
}

int main(void)
{
    int res = f(g(h(num_iterat))); // h(4) = 3; g(3) = 2; f(2) = 1

    __goblint_check(res == 1);
}