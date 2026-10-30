// PARAM: --set ana.activated[+] subpoly --set ana.activated[-] expRelation --set sem.int.signed_overflow assume_none

#include <goblint.h>

/* Disable expRelation so these checks exercise subpoly's inverse bounds.
   A bound on the old assigned variable must survive as a relation between
   the new variables. Keep the other operand unbounded so interval arithmetic
   alone cannot prove these differences. */
int main(void) {
    int x, y;
    __goblint_assume(x >= -100);
    __goblint_assume(x <= -1);
    x = x + y;
    __goblint_check(x - y >= -100); // SUCCESS
    __goblint_check(x - y <= -1);   // SUCCESS
    __goblint_check(x - y == -1);   // UNKNOWN!

    int a, b;
    __goblint_assume(a >= 0);
    __goblint_assume(a <= 10);
    a = 2 * a + b + 3;
    __goblint_check(a - b >= 3);  // SUCCESS
    __goblint_check(a - b <= 23); // SUCCESS

    int c, d;
    __goblint_assume(c >= 0);
    __goblint_assume(c <= 10);
    c = -2 * c + d + 3;
    __goblint_check(c - d >= -17); // SUCCESS
    __goblint_check(c - d <= 3);   // SUCCESS
    __goblint_check(c - d == 3);   // UNKNOWN!

    /* The assigned variable need not be first in the template's ordering. */
    int p, q;
    __goblint_assume(q >= 2);
    __goblint_assume(q <= 8);
    q = p + q + 3;
    __goblint_check(p - q >= -11); // SUCCESS
    __goblint_check(p - q <= -5);  // SUCCESS

    return 0;
}
