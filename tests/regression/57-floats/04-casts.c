// PARAM: --enable ana.int.interval --enable ana.float.interval
#include <assert.h>

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

    // Casts from double/flout/long double into different variants of ints
    assert((int)0.0);       // FAIL
    assert((long)0.0);      // FAIL
    assert((unsigned)0.0);  // FAIL
    assert((int)0.0f);      // FAIL
    assert((long)0.0f);     // FAIL
    assert((unsigned)0.0f); // FAIL
    assert((int)0.0l);      // FAIL
    assert((long)0.0l);     // FAIL
    assert((unsigned)0.0l); // FAIL

    assert((unsigned)1.0);  // SUCCESS
    assert((long)2.0);      // SUCCESS
    assert((int)3.0);       // SUCCESS
    assert((unsigned)1.0f); // SUCCESS
    assert((long)2.0f);     // SUCCESS
    assert((int)3.0f);      // SUCCESS
    assert((unsigned)1.0l); // SUCCESS
    assert((long)2.0l);     // SUCCESS
    assert((int)3.0l);      // SUCCESS

    // Cast from int into double/flaot/long double
    assert((double)0);  // FAIL
    assert((double)0l); // FAIL
    assert((double)0u); // FAIL

    assert((double)1u); // SUCCESS
    assert((double)2l); // SUCCESS
    assert((double)3);  // SUCCESS

    assert((float)0);  // FAIL
    assert((float)0l); // FAIL
    assert((float)0u); // FAIL

    assert((float)1u); // SUCCESS
    assert((float)2l); // SUCCESS
    assert((float)3);  // SUCCESS

    assert((long double)0);  // FAIL
    assert((long double)0l); // FAIL
    assert((long double)0u); // FAIL

    assert((long double)1u); // SUCCESS
    assert((long double)2l); // SUCCESS
    assert((long double)3);  // SUCCESS

    // cast with ranges
    RANGE(i, -5, 5);
    value = (double)i;
    assert(-5. <= value && value <= 5.f); // SUCCESS
    value2 = (float)i;
    assert(-5.f <= value2 && value2 <= 5.); // SUCCESS
    value3 = (long double)i;
    assert(-5.f <= value3 && value3 <= 5.l); // SUCCESS

    RANGE(l, 10, 20);
    value = l;
    assert(10.f <= value && value <= 20.); // SUCCESS
    value2 = l;
    assert(10.l <= value2 && value2 <= 20.f); // SUCCESS
    value3 = l;
    assert(10. <= value2 && value2 <= 20.); // SUCCESS

    RANGE(u, 100, 1000);
    value = u;
    assert(value > 1.); // SUCCESS
    value2 = u;
    assert(value2 > 1.f); // SUCCESS
    value3 = u;
    assert(value2 > 1.l); // SUCCESS

    RANGE(value, -10.f, 10.);
    i = (int)value;
    assert(-10 <= i && i <= 10); // SUCCESS

    RANGE(value2, -10.f, 10.);
    i = (int)value2;
    assert(-10 <= i && i <= 10); // SUCCESS

    RANGE(value3, -10.l, 10.);
    i = (int)value3;
    assert(-10 <= i && i <= 10); // SUCCESS

    return 0;
}
