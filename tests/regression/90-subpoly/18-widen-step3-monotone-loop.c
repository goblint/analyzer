// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_var
D.assign_exp
SubPolyDomain.widen
SubPolyDomain.leq
*/

int main(void) {
    int i;
    int k;

    i = k;

    while (i < 100) {
        i++;
    }

    __goblint_check(i >= k); // SUCCESS

    return 0;
}
