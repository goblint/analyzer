// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set  --set exp.unrolling-factor 3
// TODO
#include <stdio.h>

int num_iterat = 3;

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

int f(int i)
{
    if (i > 0)
    {
        for (; i >= 0; i--)
        {
            int res = g(i);
            __goblint_check(res == 1);
            return res;
        }
    }
    return -1;
}

int main(void)
{
    f(num_iterat);
    return 0;
}
