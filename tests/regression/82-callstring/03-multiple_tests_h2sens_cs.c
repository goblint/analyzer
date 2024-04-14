// PARAM: --set ana.context.callString_length 2 --set "ana.activated[+]" call_string --set ana.ctx_sens "['call_string']"  --enable ana.int.interval_set

// Test 1:
int a(int i)
{
    if (i == 0)
    {
        return 11;
    }
    if (i > 0)
    {
        return a(i - 1);
    }
    return 1;
}

int b(int i)
{
    if (i == 0)
    {
        return 12;
    }
    if (i > 0)
    {
        return a(i - 1);
    }
    return 2;
}

int c(int i)
{
    if (i == 0)
    {
        return 13;
    }
    if (i > 0)
    {
        return b(i - 1);
    }
    return 3;
}

// Test 2:
int e(int i);

int f(int i)
{
    if (i > 0)
    {
        return e(i - 1);
    }
    return 2;
}

int e(int i)
{
    return f(i - 1) + 1;
}

// Test 3:
int h(int i)
{
    if (i == 0)
    {
        return 1;
    }
    if (i > 0)
    {
        return h(i - 1);
    }
    return 0;
}

int k(int i)
{
    if (i == 2)
    {
        return h(i - 1) + 1;
    }
    if (i > 0)
    {
        return k(i - 1);
    }
    return 1;
}

// Test 4:
int q(int i);

int l(int i)
{
    if (i == 0)
    {
        return q(2);
    }
    else
    {
        return 4;
    }
}

int p(int i)
{
    return l(i - 1);
}

int s(int i)
{
    return p(i - 1);
}

int r(int i)
{
    return q(i - 1);
}

int o(int i)
{
    if (i <= 0)
    {
        return r(4);
    }
    else
    {
        return o(i - 1);
    }
}

int q(int i)
{
    if (i == 4)
    {
        return o(3);
    }
    if (i == 3)
    {
        return s(2);
    }
    return p(20);
}

int m(int i)
{
    return q(i);
}

int main(void)
{
    // Test 1:
    /* main -> a(1) -> a(0) -> return 11
       [main, a] */
    __goblint_check(a(1) == 11);

    /* main -> a(0) -> return 11
       [main] */
    __goblint_check(a(0) == 11);

    /* main -> b(0) -> return 12
       [main] */
    __goblint_check(b(0) == 12);

    /* main -> c(0) -> return 13
       [main] */
    __goblint_check(c(0) == 13);

    // Test 2:
    /* main -> f(0) -> return 2
       [main] */
    __goblint_check(f(0) == 2);

    /* main -> e(0) -> f(-1) -> return 2
       [main, e] */
    __goblint_check(e(0) == 3);

    // Test 3:
    /* main -> k(6) -> ... -> k(2) -> h(1) -> h(0) -> return 1
       [main, k] and [k, k] (3 times) and [k, h] and [h, h] */
    __goblint_check(k(6) == 2);

    // Test 4:
    /* main -> m(4) -> q(4) -> o(3) -> o(2) -> o(1) -> o(0) -> r(4) -> q(3) -> s(2) -> p(1) -> l(0) -> q(2) -> p(20) -> l(19) -> return 4
       [main, m] and [m, q] an [q, o] and [o, o] (3 times) and
       [o, r] and [r, q] and [q, s] and [s, p] and [p, l] and [l, q] and [q, p] and [p, l] */
    __goblint_check(m(4) == 4);

    return 0;
}
