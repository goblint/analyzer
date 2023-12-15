//SKIP PARAM: --set ana.activated[+] lin2vareq
// Example from https://link.springer.com/content/pdf/10.1007/BF00268497.pdf
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
