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

int g(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 1;
    }
    if (i > 0)
    {
        res = g(--i);
    }
    return res;
}

int h(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 1;
    }
    if (i > 0)
    {
        res = h(--i);
    }
    return res;
}

int main(void)
{
    int res1 = f(num_iterat);
    int res2 = g(num_iterat);
    int res3 = h(num_iterat);
    int result = res1 + res2 + res3;

    __goblint_check(result == 3); // context sensitive
}
