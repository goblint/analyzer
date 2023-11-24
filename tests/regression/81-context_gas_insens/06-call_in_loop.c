// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set
// Checks proper handling of recursion in loops + iterator variable = function parameter
#include <stdio.h>

int num_iterat = 1000; 

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
