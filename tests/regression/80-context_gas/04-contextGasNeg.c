// PARAM: --enable ana.int.interval_set --set ana.context.gas_value -5

int f(int x, int y)
{
    if (x == 0)
    {
        return y;
    }
    return f(x - 1, y - 1);
}

int main()
{
    // Context Gas lifter is disabled -> fully context-sensitive
    __goblint_check(f(20, 20) == 0);
}