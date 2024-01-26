// PARAM: --set ana.context.callStack_height 10 --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set
// Tests multiple recursive function calls
// TODO!!!
#include <stdio.h>

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

int g(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 2;
    }
    if (i > 0)
    {
        res = h(--i);
    }
    return res;
}

int f(int i)
{
    int res = 0;
    if (i == 0)
    {
        res = 3;
    }
    if (i > 0)
    {
        res = g(--i);
    }
    return res;
}

int main(void)
{
    //__goblint_check(f(-5) == 0);
    // int res1 = f(0);
    //__goblint_check(res1 == 3);
    //__goblint_check(f(1) == 2);
    //__goblint_check(f(2) == 1);
    // int res2 = f(8);
    //__goblint_check(res2 == 1); //boundary (included)
    //__goblint_check(f(9) == 1); //UNKNOWN  //boundary (excluded)
    //__goblint_check(f(20) == 1); //UNKNOWN

    //__goblint_check(g(-10) == 0);
    //__goblint_check(g(0) == 2);
    //__goblint_check(g(1) == 1);

    //__goblint_check(h(-10) == 0);
    __goblint_check(h(5) == 1);
    //__goblint_check(h(8) == 1); //boundary (included)
    ///__goblint_check(h(9) == 1); //UNKNOWN //boundary (excluded)

    // int res1 = h(5);
    // int res2 = h(4);
    // int result = res1 + res2;
    //__goblint_check(result == 2);

    //__goblint_check(f(g(h(9))) == 2);   // h(9) = 1; g(1) = 1; f(1) = 2;
    //__goblint_check(f(g(h(10))) == 2); //UNKNOWN  // h(10) = UNKNOWN
}
