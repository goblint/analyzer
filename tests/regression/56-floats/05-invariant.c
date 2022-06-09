// PARAM: --enable ana.float.interval --enable ana.int.interval
#include <assert.h>
#include <float.h>

int main()
{
    double a;
    int b;

    // make a definitly finite!
    if (b)
    {
        a = 100.;
    }
    else
    {
        a = -100.;
    };

    if (b != 1)
    {
        assert(b != 1); // SUCCESS!
    }
    else
    {
        assert(b == 1); // SUCCESS!
    }

    if (a != 1.)
    {
        // this would require a exclusion list etc.
        assert(a != 1.); // UNKNOWN!
    }

    if (a == 1.)
    {
        assert(a == 1.); // SUCCESS!
    }

    if ((int)a)
    {
        assert(a != 0.); // UNKNOWN!
    }
    if (a) // here a explicit cast to (int) should be inserted
    {
        assert(a != 0.); // UNKNOWN!
    }

    if (a <= 5.)
    {
        assert(a <= 5.); // SUCCESS!
    }

    if (a <= 5. && a >= -5.)
    {
        assert(a <= 5. && a >= -5.); // SUCCESS!
    }

    if (a + 5. < 10.)
    {
        assert(a < 5.); // SUCCESS!
    }

    if (a * 2. < 6.)
    {
        assert(a < 3.); // SUCCESS!
    }

    if (a / 3. > 10.)
    {
        assert(a > 30.); // SUCCESS!
    }

    if (((int)a) < 10)
    {
        assert(a < 10.); // SUCCESS!
    }

    int c;

    if (0.5 < (double)c)
    {
        assert(0 < c);  // SUCCESS!
        assert(1 < c);  // UNKNOWN!
        assert(1 <= c); // SUCCESS!
    }

    if (a > 1.)
    {
        assert(a < 1.); // FAIL!
        if (a < 1.)
        {
            assert(0); // NOWARN
            return 1;
        }
    }

    return 0;
}
