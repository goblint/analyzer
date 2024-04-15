// PARAM: --enable ana.int.interval_set --set ana.context.gas_value 10
#include <stdio.h>

int a = 20;

int f(int i)
{
    if (i > 0)
    {
        a = --i;
        f(i);
    }
    return 0;
}

int main(void)
{
    // main -> f(8) -> f(7) -> ... f(0)
    f(8);
    __goblint_check(a == 0);
}
