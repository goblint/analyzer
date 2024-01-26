// PARAM: --enable ana.context.ctx_gas --enable ana.int.interval_set --set ana.context.ctx_gas_value -5

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
    __goblint_check(y == 0); // UNKNOWN

    int y = f(5, 5);
    __goblint_check(y == 0); // UNKNOWN

    int y = f(1000, 1000);
    __goblint_check(y == 0); // UNKNOWN
}