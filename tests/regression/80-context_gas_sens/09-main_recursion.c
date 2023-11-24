// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set
// Checks if only recursion in main works properly + boundary check
// TODO
#include <stdio.h>

int num_iterat = 11; // WHY is context gas value + 1 the limit???

int main(void)
{
    if (num_iterat > 0)
    {
        num_iterat--;
        int res = main();
        __goblint_check(res == 5);
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
