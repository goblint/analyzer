// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_exp
D.assert_constraint
SubPolyDomain.widen
SubPolyDomain.leq

Nested loops with interacting counters: stresses widening on states whose
slack structure changes between iterations (the kind of state that triggered
Invalid_widen crashes during development). Must terminate and keep the
nonnegativity invariants.
*/

int main(void) {
    int n;
    int m;
    int i = 0;
    int j = 0;

    while (i < n) {
        j = 0;
        while (j < m) {
            j++;
        }
        __goblint_check(j >= 0); // SUCCESS
        i++;
    }

    __goblint_check(i >= 0); // SUCCESS
    __goblint_check(j >= 0); // SUCCESS

    return 0;
}
