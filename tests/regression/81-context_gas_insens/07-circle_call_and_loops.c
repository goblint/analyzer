// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set
#include <stdio.h>

int num_iterat = 20; // should be context gas value - 1

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
    int res1 = 0; 
    int res2 = 0; 
    for (int i = 2; i > 0; i--)
    {
        res1 = f(num_iterat);
        res2 = g(num_iterat);
        __goblint_check(res1 == 2); //UNKNOWN
        __goblint_check(res2 == 1); //UNKNOWN
    }

}
