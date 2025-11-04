// PARAM: --enable ana.int.interval --enable ana.float.interval
#include <goblint.h>

#define RANGE(val, min, max) \
    if (rnd)                 \
    {                        \
        val = min;           \
    }                        \
    else                     \
    {                        \
        val = max;           \
    }

int main()
{
    int rnd;

    double value;
    float value2;
    long double value3;
    int i;
    long l;
    unsigned u;

    // Casts from double/float/long double into different variants of ints
    __goblint_check((int)0.0);       // FAIL
    __goblint_check((long)0.0);      // FAIL
    __goblint_check((unsigned)0.0);  // FAIL
    __goblint_check((int)0.0f);      // FAIL
    __goblint_check((long)0.0f);     // FAIL
    __goblint_check((unsigned)0.0f); // FAIL
    __goblint_check((int)0.0l);      // FAIL
    __goblint_check((long)0.0l);     // FAIL
    __goblint_check((unsigned)0.0l); // FAIL

    __goblint_check((unsigned)1.0);  // SUCCESS
    __goblint_check((long)2.0);      // SUCCESS
    __goblint_check((int)3.0);       // SUCCESS
    __goblint_check((unsigned)1.0f); // SUCCESS
    __goblint_check((long)2.0f);     // SUCCESS
    __goblint_check((int)3.0f);      // SUCCESS
    __goblint_check((unsigned)1.0l); // SUCCESS
    __goblint_check((long)2.0l);     // SUCCESS
    __goblint_check((int)3.0l);      // SUCCESS

    // Cast from int into double/float/long double
    __goblint_check((double)0);  // FAIL
    __goblint_check((double)0l); // FAIL
    __goblint_check((double)0u); // FAIL

    __goblint_check((double)1u); // SUCCESS
    __goblint_check((double)2l); // SUCCESS
    __goblint_check((double)3);  // SUCCESS

    __goblint_check((float)0);  // FAIL
    __goblint_check((float)0l); // FAIL
    __goblint_check((float)0u); // FAIL

    __goblint_check((float)1u); // SUCCESS
    __goblint_check((float)2l); // SUCCESS
    __goblint_check((float)3);  // SUCCESS

    __goblint_check((long double)0);  // FAIL
    __goblint_check((long double)0l); // FAIL
    __goblint_check((long double)0u); // FAIL

    __goblint_check((long double)1u); // SUCCESS
    __goblint_check((long double)2l); // SUCCESS
    __goblint_check((long double)3);  // SUCCESS

    // cast with ranges
    RANGE(i, -5, 5);
    value = (double)i;
    __goblint_check(-5. <= value && value <= 5.f); // SUCCESS
    value2 = (float)i;
    __goblint_check(-5.f <= value2 && value2 <= 5.); // SUCCESS
    value3 = (long double)i;
    __goblint_check(-5.f <= value3 && value3 <= 5.l); // SUCCESS

    RANGE(l, 10, 20);
    value = l;
    __goblint_check(10.f <= value && value <= 20.); // SUCCESS
    value2 = l;
    __goblint_check(10.l <= value2 && value2 <= 20.f); // SUCCESS
    value3 = l;
    __goblint_check(10. <= value2 && value2 <= 20.); // SUCCESS

    RANGE(u, 100, 1000);
    value = u;
    __goblint_check(value > 1.); // SUCCESS
    value2 = u;
    __goblint_check(value2 > 1.f); // SUCCESS
    value3 = u;
    __goblint_check(value2 > 1.l); // SUCCESS

    RANGE(value, -10.f, 10.);
    i = (int)value;
    __goblint_check(-10 <= i && i <= 10); // SUCCESS

    RANGE(value2, -10.f, 10.);
    i = (int)value2;
    __goblint_check(-10 <= i && i <= 10); // SUCCESS

    RANGE(value3, -10.l, 10.);
    i = (int)value3;
    __goblint_check(-10 <= i && i <= 10); // SUCCESS

    return 0;
}
