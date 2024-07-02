// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts
// example of the paper "2-Pointer Logic" by Seidl et al., Example 9, pag. 22
#include <stdlib.h>
#include <goblint.h>

void main(void) {
    long x;
    long *z;
    z = &x;
    long y;

    y = -1 + x;

    __goblint_check(z == &x);
    __goblint_check(y == -1 + x);

    *z = 1 + x;

    __goblint_check(&x == z);
    __goblint_check(y == -2 + x);

}
