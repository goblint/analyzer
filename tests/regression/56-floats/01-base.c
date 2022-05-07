#include <assert.h>
#include <float.h>
#include <limits.h>
#include <math.h>

int main()
{
    double middle;
    double a = 2.;
    double b = 4.;

    assert(middle == 2.); // UNKNOWN!

    assert(a == 2.); // SUCCESS!
    assert(a < 10.); // SUCCESS!
    assert(a > 10.); // FAIL!

    middle = (a + b) / 2.; // naive way of computing the middle

    assert(middle == 3.); // SUCCESS!

    assert(-97. == middle - 100.);
    return 0;
}
