//SKIP PARAM: --set ana.activated[+] lin2vareq

// Example from https://link.springer.com/content/pdf/10.1007/BF00268497.pdf --set ana.int.def_exc false --set ana.int.enums false --set ana.int.interval false --set ana.int.interval_set false --set ana.int.congruence false

#include <goblint.h>

void main(void) {
    int i;
    int k;
    i = 2;
    k = 0;

    while (i < 100) {
        __goblint_check(3 * i - k == 1); //UNKNOWN!
        i = i + 1;
        k = k + 3;
    }
    __goblint_check(3 * i - k == 1); //UNKNOWN!

}
