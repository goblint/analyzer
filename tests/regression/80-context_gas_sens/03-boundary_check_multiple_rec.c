// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set
// Interesting if multiple recursions are handled properly + boundary check for multiple recursions
#include <stdio.h>

int num_iterat = 8; // should be context gas value - 2

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
        res = g(--i);
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
        res = h(--i);
    }
    return res;
}

int main(void)
{
    int res1 = f(num_iterat);
    int res2 = g(num_iterat);
    int res3 = h(num_iterat);
    int res4 = h(num_iterat);
    int res5 = h(num_iterat);
    int result = res1 + res2 + res3 + res4 + res5;

    __goblint_check(result == 5); 
}