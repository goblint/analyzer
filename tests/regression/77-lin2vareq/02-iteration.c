//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main() {
    int i, j;
    int size = 5;

    for (i = 0; i < size; ++i) {
        j = i;

       __goblint_check(i == j); //UNKNOWN!
     }

    return 0;
}
