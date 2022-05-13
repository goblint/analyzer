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
    // Casts from double into different variants of ints
    assert((int)0.0);      // FAIL!
    assert((long)0.0);     // FAIL!
    assert((unsigned)0.0); // FAIL!

    assert((unsigned)1.0); // SUCCESS!
    assert((long)2.0);     // SUCCESS!
    assert((int)3.0);      // SUCCESS!

    // Cast from int into double
    assert((double)0);  // FAIL!
    assert((double)0l); // FAIL!
    assert((double)0u); // FAIL!

    assert((double)1u); // SUCCESS!
    assert((double)2l); // SUCCESS!
    assert((double)3);  // SUCCESS!

    // cast with ranges
    int rnd;

    double value;
    int i;
    long l;
    unsigned u;

    RANGE(i, -5, 5);
    value = (double)i;
    assert(-5. <= value && value <= 5.); // SUCCESS!

    RANGE(l, 10, 20);
    value = l;
    assert(10. <= value && value <= 20.); // SUCCESS!

    RANGE(u, 100, 1000);
    value = u;
    assert(value > 1.); // SUCCESS!

    RANGE(value, -10., 10.);
    i = (int)value;
    assert(-10 <= i && i <= 10); // SUCCESS!

    return 0;
}
