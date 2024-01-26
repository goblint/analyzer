// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set
// Without context insensitive analysis: Stack Overflow
#include <stdio.h>

int num_iterat = 200000;

int main(void)
{
    if (num_iterat > 0)
    {
        num_iterat--;
        int res = main();
        __goblint_check(res == 5); // UNKNOWN
        return res;
    }
    else
    {
        if (num_iterat == 0)
        {
            return 5;
        }
        return 2;
    }
}
