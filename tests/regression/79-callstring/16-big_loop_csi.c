// PARAM: --set ana.context.callString_length 10 --set "ana.activated[+]" call_site --set ana.ctx_sens "['call_site']" --enable ana.int.interval_set
// Checks proper handling of recursions in loops + shows that not all 200 iterations are analyzed

int f(int i);

int g(int i)
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

int f(int i)
{
    if (i == 0)
    {
        return 2;
    }
    if (i > 0)
    {
        return g(i - 1);
    }
    return 12;
}

int main(void)
{
    for (int i = 200; i > 0; i--)
    {
        int res1 = f(2);
        int res2 = g(2);
        __goblint_check(res1 == 2);
        __goblint_check(res2 == 1);
    }
}
