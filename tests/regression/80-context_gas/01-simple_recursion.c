// PARAM: --enable ana.opt.ctx_gas --enable ana.int.interval_set 
#include <stdio.h>

int num_iterat = 5;
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

    // if the analysis runs fully context sensitive, "a" should be equal 0
    __goblint_check(a == 0);
}
