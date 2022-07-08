// PARAM: --enable ana.float.interval --enable ana.int.interval
#include <assert.h>
#include <float.h>
#include <math.h>

int main()
{
    double a;
    int d;

    // a pretty high number
    double high_number = 340281981556000088756250604298070654976.;
    double next = 340281981556000126535182467255232364544.;
    double distance_to_next = next - high_number;
    assert(high_number + distance_to_next == next); // SUCCESS

    // make a definitly finite!
    if (d)
    {
        a = 2 * high_number;
    }
    else
    {
        a = -2 * high_number;
    };

    if (a + high_number <= high_number)
    {
        // the mathematical solution with `a <= 0` is obviously not the only one here
        assert(a <= 0.); // UNKNOWN!
        // assuming the rounding mode "nearest", `a` had to be below `distance_to_next / 2`
        // (as half-way cases are always rounded away from zero)
        assert(a < (distance_to_next / 2)); // UNKNOWN!
        // ... this than also has to be unknown
        assert(a <= (distance_to_next / 2)); // UNKNOWN!
        // with up/down as rounding mode -> a only has to be below `distance_to_next`
        assert(a < distance_to_next);  // UNKNOWN
        assert(a <= distance_to_next); // SUCCESS
    }

    double one = 1.;
    if (a * one >= next)
    {
        // the mathematical solution again would be `a >= next` but does not take the rounding mode into account
        assert(a >= next); // UNKNOWN!
        // assuming the rounding mode "nearest", `a` had to be at least `high_number + distance_to_next / 2`
        // (as half-way cases are always rounded away from zero)
        assert(a - high_number >= (distance_to_next / 2)); // UNKNOWN!
        // ... this than also has to be unknown
        assert(a - high_number > (distance_to_next / 2)); // UNKNOWN!
        // with up/down as rounding mode -> a only has to be above `high_number`
        assert(a > high_number);  // UNKNOWN
        assert(a >= high_number); // SUCCESS
    }

    return 0;
}
