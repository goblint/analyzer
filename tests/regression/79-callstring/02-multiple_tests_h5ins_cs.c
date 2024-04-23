// PARAM: --set ana.context.callString_length 5 --set "ana.activated[+]" call_string --set ana.ctx_sens "['call_string']" --enable ana.int.interval_set

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
int m = 20;

int o(int i)
{
    if (i > 0)
    {
        m = --i;
        o(i);
    }
    return 0;
}

int main(void)
{
    // Test 1:
    /* main -> a(8) -> ... -> a(0) -> return 11
       [main, a, a, a, a] and [a, a, a, a, a] (4 times) */
    __goblint_check(a(8) == 11); // UNKNOWN

    /* main -> b(21) -> a(20) -> ... -> a(1) -> a(0) -> return 11
       [main, b, a, a, a] and [g, a, a, a, a] and [a, a, a, a, a] (16 times) */
    __goblint_check(b(21) == 11); // UNKNOWN

    /* main -> c(11) -> b(10) -> a(9) -> ... -> a(0) -> return 11
       [main, c, b, a, a] and [c, b, a, a, a] and [b, a, a, a, a] and [a, a, a, a, a] (5 times) */
    __goblint_check(c(11) == 11); // UNKNOWN

    // Test 2:
    /* main -> e(7) -> f(6) -> e(5) -> ... -> e(1) -> f(0) -> return 2
       [main, e, f, e, f] and [e, f , e, f, e] (2 times) and [f , e, f, e, f] */
    __goblint_check(e(7) == 6); // UNKNOWN

    // Test 3:
    /* main -> k(10) -> ... -> k(2) -> h(1) -> h(0) -> return 1
       [main, k, k, k, k] and [k, k, k, k, k] (4 times) and [k, k, k, k, h] */
    __goblint_check(k(10) == 2); // UNKNOWN

    // Test 4:
    /* main -> o(6) -> o(5) -> ... o(0)
       [main, o, o, o, o] and [o, o, o, o, o] (2 times) */
    o(6);
    __goblint_check(m == 0); // UNKNOWN

    return 0;
}
