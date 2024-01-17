//SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none
#include <stdio.h>

int main() {
    int i, j, k;
    int size = 5;

    for (i = 0; i < size; ++i) {
        j = 2 * i;
        k = j + i;

        __goblint_check(j == 2 * i); //UNKNOWN!

        __goblint_check(k == j + i); //UNKNOWN!

        __goblint_check(k == 3 * i); //UNKNOWN!
    }

    return 0;
}
