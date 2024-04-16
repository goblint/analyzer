// PARAM: --set ana.activated[+] wrpointer
// example of the paper "2-Pointer Logic" by Seidl et al., pag. 22
#include <goblint.h>

void main(void) {
    int x;
    int *z = &x;
    int y = -1 + x;

    __goblint_check(z == &x);
    __goblint_check(y == -1 + x);

    *z = 1 + x,

    __goblint_check(&x == z);
    __goblint_check(y == -2 + x);

}
