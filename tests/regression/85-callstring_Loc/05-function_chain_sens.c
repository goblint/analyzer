// PARAM: --set "ana.activated[+]" callstring_loc --enable ana.int.interval_set
// Checks if function chains are handled properly
#include <stdio.h>

int num_iterat = 12;

int h(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 1;
    }
    if (i > 0)
    {
        res = h(--i);
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
        res = h(--i);
    }
    return res;
}

int f(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 3;
    }
    if (i > 0)
    {
        res = g(--i);
    }
    return res;
}

int main(void)
{
    int result = f(num_iterat);
    
    __goblint_check(result == 1); 
}
