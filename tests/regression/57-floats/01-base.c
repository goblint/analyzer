// PARAM: --enable ana.float.interval
#include <assert.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <complex.h>

int main()
{
    // ensure that complex floats just do not do anything
    double _Complex cplx = 0.;
    __goblint_check(cplx == 0.); // UNKNOWN

    double x, a = 2., b = 3. + 1;
    float y, c = 2.f, d = 3.f + 1;
    long double z, e = 2.l, f = 3.l + 1;

    __goblint_check(x == 2.);  // UNKNOWN!
    __goblint_check(y == 2.f); // UNKNOWN!
    __goblint_check(z == 2.l); // UNKNOWN!

    __goblint_check(a == 2.); // SUCCESS
    __goblint_check(a < 10.); // SUCCESS
    __goblint_check(a > 10.); // FAIL

    __goblint_check(c == 2.f); // SUCCESS
    __goblint_check(c < 10.f); // SUCCESS
    __goblint_check(c > 10.f); // FAIL

    __goblint_check(e == 2.f); // SUCCESS
    __goblint_check(e < 10.f); // SUCCESS
    __goblint_check(e > 10.f); // FAIL

    x = (a + b) / 2.; // naive way of computing the middle
    y = (c + d) / 2.; // naive way of computing the middle
    z = (e + f) / 2.; // naive way of computing the middle

    __goblint_check(x == 3.);  // SUCCESS
    __goblint_check(y == 3.f); // SUCCESS
    __goblint_check(z == 3.f); // SUCCESS

    __goblint_check(-97. == x - 100.);
    __goblint_check(-97.f == y - 100.f);
    __goblint_check(-97.f == z - 100.f);
    return 0;
}
