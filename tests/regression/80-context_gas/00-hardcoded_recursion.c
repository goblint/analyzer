// PARAM: --enable ana.int.interval_set --enable ana.int.def_exc --enable ana.int.enums
#include <stdio.h>

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
}

int main(void)
{
    f(5);

    // if the analysis runs fully context sensitive, "a" should be equal 0
    __goblint_check(a == 0);
}
