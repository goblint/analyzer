// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_var_parallel
D.assign_exp
D.assert_constraint
SubPolyDomain.join
SubPolyDomain.widen
SubPolyDomain.set_slack
RationalInterval.join
ExpressionBounds.bound_texpr
*/

int normalize_pair(int lo, int hi, int limit, int choose) {
    int left;
    int right;
    int i = 0;

    __goblint_assume(lo <= hi);
    __goblint_assume(limit >= 0);

    if (choose) {
        left = lo;
        right = hi;
    } else {
        left = lo + 1;
        right = hi + 1;
    }

    while (i < limit) {
        left++;
        right++;
        i++;
    }

    __goblint_check(left <= right);              // SUCCESS
    __goblint_check(right - left == hi - lo);    // SUCCESS
    __goblint_check(i >= 0);                     // SUCCESS
    __goblint_check(i == limit);                 // UNKNOWN!

    return right - left;
}

int main(void) {
    int a;
    int b;
    int n;
    int c;
    int d = normalize_pair(a, b, n, c);

    __goblint_check(d >= 0); // SUCCESS

    return 0;
}
