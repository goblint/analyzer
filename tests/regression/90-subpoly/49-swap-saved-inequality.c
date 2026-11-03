// PARAM: --set ana.activated[+] subpoly --set ana.activated[-] expRelation --enable dbg.subpoly.check-invariants

#include <goblint.h>

int main(void) {
    unsigned int a, b;
    if (a < b) {
        unsigned int saved = a;
        a = b;
        // Overwriting a must retain old a < b through saved == old a.
        __goblint_check(saved < a); // SUCCESS
        b = saved;
        __goblint_check(a > b); // SUCCESS
    }

    __goblint_check(a >= b); // SUCCESS
    __goblint_check(a == b); // UNKNOWN!
    return 0;
}
