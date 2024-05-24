// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts
// example of the paper "2-Pointer Logic" by Seidl et al., pag. 21
#include <goblint.h>

void main(void) {
    long x;
    long *z = -1 + &x;

    __goblint_check(z == -1 + &x);

    z = (long*) *(1 + z);

    __goblint_check(x == (long)z);

}
