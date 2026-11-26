// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_var_parallel
D.assign_var_parallel_with
D.assign_exp
D.assert_constraint
SubPolyDomain.join
SubPolyDomain.widen
SubPolyDomain.leq
ExpressionBounds.bound_texpr
*/

int accumulate_same_delta(int start, int stop, int step, int seed) {
    int i = start;
    int total = seed;

    __goblint_assume(step >= 1);
    __goblint_assume(start <= stop);

    while (i < stop) {
        i = i + step;
        total = total + step;
    }

    __goblint_check(i >= start);                    // SUCCESS
    __goblint_check(total - seed == i - start);     // SUCCESS
    __goblint_check(total >= seed);                 // SUCCESS
    __goblint_check(i == stop);                     // UNKNOWN!

    return total;
}

int main(void) {
    int a;
    int b;
    int s;
    int base;
    int r = accumulate_same_delta(a, b, s, base);

    __goblint_check(r >= base); // SUCCESS

    return 0;
}
