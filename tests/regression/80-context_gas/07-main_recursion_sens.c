// PARAM: --enable ana.int.interval_set --set ana.context.gas_value 10
// Note: 11 function calls are possible and the analysis is still context-sensitive since the domain tracks the parameter value
#include <stdio.h>

int num_iterat = 11;

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
