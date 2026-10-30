// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/* Bounds on differences require relational information: base intervals alone
   cannot prove the refinements produced by these disequality guards. */
int main(void) {
    int x, y;
    __goblint_assume(x - y >= 0);
    __goblint_assume(x - y <= 5);
    if (x != y) {
        // Excluding the lower endpoint leaves [1, 5].
        __goblint_check(x - y >= 1); // SUCCESS
        __goblint_check(x - y <= 5); // SUCCESS
        __goblint_check(x - y == 1); // UNKNOWN!
    }

    int a, b;
    __goblint_assume(a - b >= 0);
    __goblint_assume(a - b <= 5);
    if (a - b != 5) {
        // Excluding the upper endpoint leaves [0, 4].
        __goblint_check(a - b >= 0); // SUCCESS
        __goblint_check(a - b <= 4); // SUCCESS
        __goblint_check(a - b == 4); // UNKNOWN!
    }

    int p, q;
    __goblint_assume(p - q >= -5);
    __goblint_assume(p - q <= 5);
    if (p != q) {
        // Excluding an interior point must not select either sign.
        __goblint_check(p > q); // UNKNOWN!
        __goblint_check(p < q); // UNKNOWN!
    }
    return 0;
}
