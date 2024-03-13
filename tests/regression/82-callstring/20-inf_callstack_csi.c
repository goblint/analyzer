// PARAM: --set "ana.activated[+]" call_site --set ana.ctx_sens "['call_site']"  --enable ana.int.interval_set  --set ana.context.callStack_height -1

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
    f(25);
    __goblint_check(a == 0);
}
