// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assert_constraint
ExpressionBounds.bound_texpr
P.eval_vec

Inequalities with non-unit coefficients: the stored template must answer
queries for itself, and the reduction must combine the slack bound with a
variable bound to derive bounds on a single variable (x = (beta + 3y)/2).
*/

int main(void) {
    int x;
    int y;

    __goblint_assume(2 * x - 3 * y <= 7);
    __goblint_assume(y <= 1);

    __goblint_check(2 * x - 3 * y <= 7); // SUCCESS
    __goblint_check(x <= 5);             // SUCCESS
    __goblint_check(x <= 4);             // UNKNOWN!

    return 0;
}
