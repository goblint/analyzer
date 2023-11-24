// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set
// Checks if recursion in loop is handled properly + iterator variable = function parameter
// TODO
#include <stdio.h>

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
    int res = 0;
    int result = 0;
    for (int i = 5; i > 0; i--)
    {
        res = f(i);
        __goblint_check(res == 1); // TODO
        result += res;
    }
    __goblint_check(result == 5); // TODO
}
