// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set --set exp.unrolling-factor 3
// Checks if recursion in loops are handled properly + loop unrolling
// TODO
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

int main(void)
{
    int res = 0;
    int result = 0;
    for (int i = 5; i > 0; i--)
    {
        res = f(num_iterat);
        __goblint_check(res == 1); 
        result += res;
    }
    __goblint_check(result == 5); // TODO
}
