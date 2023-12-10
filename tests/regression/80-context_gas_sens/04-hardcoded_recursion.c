// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set
// Interesting if the context gas is reduced too much, due to the (not needed) function calls f(4) and f(3)
#include <stdio.h>

int num_iterat = 2;
int a = 20;

int f(int i)
{
    if (i == 5)
    {
        a = 4;
        f(4);
    }
    if (i == 4)
    {
        a = 3;
        f(3);
    }
    if (i == 3)
    {
        a = 2;
        f(2);
    }
    if (i == 2)
    {
        a = 1;
        f(1);
    }
    if (i == 1)
    {
        a = 0;
        f(0);
    }
    if (i == 0)
    {
        return 0;
    }
}

int main(void)
{
    f(num_iterat);

    // if the analysis runs fully context sensitive, "a" should be equal 0
    __goblint_check(a == 0);
}