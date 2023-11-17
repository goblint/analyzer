// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set 
// Basic example + boundary check for simple recursion
#include <stdio.h>

int num_iterat = 9; // should be context gas value - 1
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
    f(num_iterat);
    __goblint_check(a == 0); // UNKNOWN
}
