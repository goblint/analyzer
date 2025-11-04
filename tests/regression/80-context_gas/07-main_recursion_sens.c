// PARAM: --enable ana.int.interval_set --set ana.context.gas_value 10
// Note: 11 function calls are analyzed context-sensitively
// -> tracked parameter in domain enables one additional context-sensitively analyzed call value
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
