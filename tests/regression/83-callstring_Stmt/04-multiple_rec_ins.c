// PARAM: --enable ana.context.callstring_stmt --enable ana.int.interval_set
// Interesting if multiple recursions are handled properly
#include <stdio.h>

int num_iterat = 11;

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
    __goblint_check(result == 5); //UNKNOWN
}
