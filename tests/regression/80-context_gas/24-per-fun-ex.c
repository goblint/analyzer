// PARAM: --enable ana.int.interval_set --set ana.context.gas_value 3 --set ana.context.gas_scope function
#include <goblint.h>

int nr(int x, int y) {
    // Non-recursive, but would fail with global scope as gas for f is exhausted
    __goblint_check(x==y);
}

// Checks that gas is also applied to further functions
int g(int x, int y)
{
    int top;
    if (x < -100000)
    {
        return y;
    }

    if(top) {
        nr(5,5);
    } else {
        nr(6,6);
    }

    return g(x - 1, y - 1);
}

int f(int x, int y)
{
    int top;

    if (x == 0)
    {
        return y;
    }

    g(x,y);

    return f(x - 1, y - 1);
}

int main()
{
    // main -> f(3,3) -> f(2,2) -> f(1,1) -> f(0,0) -> return 0
    // 4 recursive calls -> boundary (excluded)
    __goblint_check(f(3, 3) == 0); // UNKNOWN
}
