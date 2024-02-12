// PARAM: --set ana.context.callStack_height 10 --set "ana.activated[+]" callstring_fundec --enable ana.int.interval_set

int f(int i);

int g(int i)
{
    if (i > 0)
    {
        return f(i - 1);
    }
    return 2;
}

int f(int i)
{
    return g(i - 1);
}

int main()
{
    // main -> f(7) -> g(6) -> f(5) -> ... -> f(1) -> g(0) -> return 2
    // [main, f, g, f, g, f, g, f, g]
    __goblint_check(f(7) == 2);
    return 0;
}
