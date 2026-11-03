// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.forget_vars
P.rescue_dependent_slacks
SubPolyDomain.meet

Unknown extern calls: value-passed locals keep their relations, an
address-taken local is havocked (forget path through invalidation), and the
unknown return value stays unconstrained. Must not crash and must not
retain stale facts.
*/

extern int mystery(int v);
extern void mystery_ptr(int *p);

int main(void) {
    int x;
    int y;

    __goblint_assume(x == y + 2);

    int r = mystery(x);
    __goblint_check(x == y + 2); // SUCCESS
    __goblint_check(r == x);     // UNKNOWN!

    int a = 5;
    mystery_ptr(&a);
    __goblint_check(a == 5); // UNKNOWN!

    return 0;
}
