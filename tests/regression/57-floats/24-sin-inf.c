// PARAM: --enable ana.float.interval
#include <math.h>
#include <float.h>
#include <goblint.h>

int main()
{

    double max_double = DBL_MAX;

    double res = sin(max_double);
    __goblint_check(-1. <= res); //UNKNOWN!
    __goblint_check(res <= 1.); //UNKNOWN!

    if(__builtin_isnan(res)) {
        __goblint_check(1); // Reachable
    }

    res = cos(max_double);
    __goblint_check(-1. <= res); //UNKNOWN!
    __goblint_check(res <= 1.); //UNKNOWN!

    if(__builtin_isnan(res)) {
        __goblint_check(1); // Reachable
    }

    float max_float = FLT_MAX;

    float resf = sinf(max_double);
    __goblint_check(-1. <= resf); //UNKNOWN!
    __goblint_check(resf <= 1.); //UNKNOWN!

    if(__builtin_isnan(resf)) {
        __goblint_check(1); // Reachable
    }

    resf = cosf(max_double);
    __goblint_check(-1. <= resf); //UNKNOWN!
    __goblint_check(resf <= 1.); //UNKNOWN!

    if(__builtin_isnan(resf)) {
        __goblint_check(1); // Reachable
    }
}
