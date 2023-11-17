// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set
#include <stdio.h>

int num_iterat = 1000; // should be context gas value - 1

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
    for (int i = num_iterat; i > 0; i--)
    {
        int result = f(i);
        __goblint_check(result == 1); // UNKNOWN
    }
}
