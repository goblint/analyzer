// PARAM: --enable ana.opt.ctx_gas --enable ana.int.interval_set
int num_iterat = 10;

int f(int x, int y)
{
    if (x == 0)
    {
        __goblint_check(y == 0);
        return 0;
    }
    f(--x, --y);
}

int main()
{
    f(num_iterat, num_iterat);
}