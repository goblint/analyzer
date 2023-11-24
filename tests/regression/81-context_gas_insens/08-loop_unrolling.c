// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set --set exp.unrolling-factor 3
// Checks if loop unrolling makes a difference for the analysis
#include <stdio.h>

int num_iterat = 20; 

int f(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 2;
    }
    if (i > 0)
    {
        res = f(--i);
    }
    return res;
}

int main(void)
{
    for (int i = 5; i > 0; i--)
    {
        int res = f(num_iterat);
        __goblint_check(res == 2); //UNKNOWN
    }

}