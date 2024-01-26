// PARAM: --set ana.context.callStack_height 10 --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set
// This code would result in a StackOverflow if it is analyses fully context sensitive
#include <stdio.h>

int ack(int n, int m)
{
    if (n == 0)
    {
        return m + 1;
    }
    else
    {
        if (m == 0)
        {
            return ack(n - 1, 1);
        }
        else
        {
            return ack(n - 1, ack(n, m - 1));
        }
    }
}

int main(void)
{
    ack(4, 1);
    __goblint_check(1); // reachable
}