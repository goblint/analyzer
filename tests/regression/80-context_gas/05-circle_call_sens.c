// PARAM: --enable ana.int.interval_set --set ana.context.gas_value 15
// Checks if recursion in loops is handled properly
#include <stdio.h>

int f(int i);

int g(int i)
{
    if (i == 0)
    {
        return 1;
    }
    if (i > 0)
    {
        return f(i + 1);
    }
    return 11;
}

int f(int i)
{
    if (i == 0)
    {
        return 2;
    }
    if (i > 0)
    {
        return g(i - 2) + g(i - 3);
    }
    return 12;
}

int main(void)
{
    __goblint_check(f(0) == 2);
    __goblint_check(f(7) == 101);
    // f(9) -> g(7) -> f(8) -> g(6) -> f(7) -> g(5) -> f(6) -> g(4) -> f(5) -> g(3) -> f(4) -> g(2) -> f(3) -> g(1) -> f(2) -> g(0) -> return 
    // f(9) -> g(6) -> f(7) -> g(4) -> f(5) -> g(2) -> f(3) -> g(0)
    __goblint_check(f(9) == 265); 
    __goblint_check(f(10) == 429);

    __goblint_check(g(0) == 1);
    __goblint_check(g(3) == 25);
    __goblint_check(g(6) == 101);
    __goblint_check(g(8) == 265);
}
