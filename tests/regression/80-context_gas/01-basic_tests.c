// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set --set ana.context.ctx_gas_value 10
// Basic examples

int f(int x, int y)
{
    if (x == 0)
    {

        return y;
    }
    return f(--x, --y);
}

int main()
{
    int y = f(0, 0);
    __goblint_check(y == 0);

    int y = f(5, 5);
    __goblint_check(y == 0);

    int y = f(7, 7);
    __goblint_check(y == 0); //boundary (included)

    int y = f(8, 8);
    __goblint_check(y == 0); //UNKNOWN //boundary (excluded)

    int y = f(10, 10);
    __goblint_check(y == 0); //UNKNOWN

    int y = f(1000, 1000);
    __goblint_check(y == 0); //UNKNOWN
}