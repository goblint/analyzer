// PARAM: --enable ana.int.interval_set --set ana.context.gas_value 10

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
    // main -> f(9,9) -> f(8,8) -> f(7,7) -> g(6,6) -> f(5,5) -> f(4,4) -> f(3,3) -> f(2,2) -> f(1,1) -> f(0,0) -> return 0
    // 11 functions -> boundary (excluded)
    __goblint_check(f(9, 9) == 0); // UNKNOWN
}