// PARAM: --set "ana.activated[+]" loopfree_callstring --enable ana.int.interval_set
// Basic example
#include <stdio.h>

int g(int i);

int c(int i)
{
    if (i == 0)
    {
        return g(2);
    }
    else
    {
        return 4;
    }
}

int b(int i)
{
    return c(i - 1);
}

int a(int i)
{
    return b(i - 1);
}

int h(int i)
{
    return g(i - 1);
}

int f(int i)
{
    if (i <= 0)
    {
        return h(4);
    }
    else
    {
        return f(i - 1);
    }
}

int g(int i)
{
    if (i == 4)
    {
        return f(3);
    }
    if (i == 3)
    {
        return a(2);
    }
    return b(20);
}

int m(int i)
{
    return g(i);
}

int main(void)
{
    // main -> m(4) -> g(4) -> f(3) -> f(2) -> f(1) -> f(0) -> h(4) -> g(3) -> a(2) -> b(1) -> c(0) -> g(2) -> b(20) -> c(19) -> return 4
    // m:       [main, m]
    // g:       [main, m, g]
    // f:       [main, m, g, f]
    // f:       [main, m, g, {f}] (3 times)
    // h:       [main, m, g, {f}, h]
    // g:       [main, m, {g, f, h}]
    // a:       [main, m, {g, f, h}, a]
    // b:       [main, m, {g, f, h}, a, b]
    // c:       [main, m, {g, f, h}, a, b, c]
    // g, b, c: [main, m, {g, f, h, a, b, c}} (3 times)
    __goblint_check(m(4) == 4);
}
