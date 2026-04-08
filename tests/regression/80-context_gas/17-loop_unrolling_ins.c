// PARAM: --enable ana.int.interval_set --set exp.unrolling-factor 3 --set ana.context.gas_value 10
// Note: 11 function calls are analyzed context-sensitively
// -> tracked parameter in domain enables one additional context-sensitively analyzed call
#include <stdio.h>

int f(int i)
{
    if (i == 0)
    {
        return 11;
    }
    if (i > 0)
    {
        return f(i - 1);
    }
    return 1;
}

int main(void)
{

    for (int i = 5; i > 0; i--)
    {
        __goblint_check(f(11) == 11); // UNKNOWN
    }
}
