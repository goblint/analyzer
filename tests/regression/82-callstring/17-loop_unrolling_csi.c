// PARAM: --set ana.context.callStack_height 2 --set "ana.activated[+]" call_site --enable ana.int.interval_set --set exp.unrolling-factor 2

int f(int i)
{
    if (i == 0)
    {
        return 1;
    }
    if (i > 0)
    {
        return f(i - 1);
    }
    return 11;
}

int main(void)
{
    for (int i = 5; i > 0; i--)
    {
        // main -> f(4) -> ... -> f(0) -> return 1
        __goblint_check(f(4) == 1);
    }
}
