// PARAM: --enable ana.float.interval --enable ana.int.interval
#include <goblint.h>
#include <float.h>

int main()
{
    double a;
    float b;
    long double c;
    int d;

    // make a definitly finite!
    if (d)
    {
        a = 100.;
        b = 100.;
        c = 100.;
    }
    else
    {
        a = -100.;
        b = -100.;
        c = -100.;
    };

    if (a != 1.)
    {
        // this would require a exclusion list etc.
        __goblint_check(a != 1.); // UNKNOWN
    }

    if (a == 1.)
    {
        __goblint_check(a == 1.); // SUCCESS
    }

    if (b == 1.f)
    {
        __goblint_check(b == 1.f); // SUCCESS
    }

    if (a <= 5.)
    {
        __goblint_check(a <= 5.); // SUCCESS
    }
    if (b <= 5.f)
    {
        __goblint_check(b <= 5.f); // SUCCESS
    }
    if (c <= 5.f)
    {
        __goblint_check(c <= 5.f); // SUCCESS
    }

    if (a <= 5. && a >= -5.)
    {
        __goblint_check(a <= 5. && a >= -5.); // SUCCESS
    }
    if (b <= 5.f && b >= -5.f)
    {
        __goblint_check(b <= 5. && b >= -5.); // SUCCESS
    }

    if (a + 5.f < 10.f)
    {
        __goblint_check(a <= 5.); // SUCCESS
    }
    if (b + 5.f < 10.f)
    {
        __goblint_check(b <= 5.l); // SUCCESS
    }
    if (c + 5.f < 10.f)
    {
        __goblint_check(c <= 5.f); // SUCCESS
    }

    if (a * 2. < 6.f)
    {
        __goblint_check(a <= 3.); // SUCCESS
    }
    if (b * 2.f < 6.f)
    {
        __goblint_check(b <= 3.f); // SUCCESS
    }
    if (c * 2. < 6.f)
    {
        __goblint_check(c <= 3.f); // SUCCESS
    }

    if (a / 3. > 10.)
    {
        __goblint_check(a >= 30.); // SUCCESS
    }
    if (b / 3.f > 10.f)
    {
        __goblint_check(b >= 30); // SUCCESS
    }

    if (a < 10)
    {
        __goblint_check(a < 10.); // SUCCESS
    }
    if (b < 10)
    {
        __goblint_check(b < 10.f); // SUCCESS
    }
    if (c < 10)
    {
        __goblint_check(c < 10.l); // SUCCESS
    }

    if (a > 1.)
    {
        __goblint_check(a < 1.); // FAIL
        if (a < 1.)
        {
            __goblint_check(0); // NOWARN
            return 1;
        }
    }

    return 0;
}
