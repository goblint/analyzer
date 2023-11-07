// PARAM: --enable ana.int.interval_set --enable ana.int.def_exc --enable ana.int.enums
#include <stdio.h>

int num_iterat = 5;

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
    int res1 = f(num_iterat);
    int res2 = f(num_iterat);
    int result = res1 + res2;

    __goblint_check(result == 2); // context sensitive
}