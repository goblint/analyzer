// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

static void negated_guard(void) {
    int x, y;
    // The continuing edge negates an already negated comparison.
    if (!(x < y)) return;
    __goblint_check(x < y); // SUCCESS
    if (x < y) x++;
    if (x < y) return;
    __goblint_check(x == y); // SUCCESS
}

int main(void) {
    int x, y;
    if (!(x < y)) {
        __goblint_check(x >= y); // SUCCESS
        __goblint_check(x == y); // UNKNOWN!
    }

    if (x >= y && x <= y + 2) {
        __goblint_check(x - y >= 0); // SUCCESS
        __goblint_check(x - y <= 2); // SUCCESS
    }

    if (!(x < y || x > y + 2)) {
        __goblint_check(x - y >= 0); // SUCCESS
        __goblint_check(x - y <= 2); // SUCCESS
    }

    if (x == y + 1 || x == y + 3) {
        __goblint_check(x - y >= 1); // SUCCESS
        __goblint_check(x - y <= 3); // SUCCESS
        __goblint_check(x - y == 1); // UNKNOWN!
        __goblint_check(x - y == 3); // UNKNOWN!
    }

    if (!(x < y && x > y - 2)) {
        // Both alternatives must survive the disjunctive approximation.
        __goblint_check(x >= y); // UNKNOWN!
        __goblint_check(x <= y - 2); // UNKNOWN!
    }

    negated_guard();
    return 0;
}
