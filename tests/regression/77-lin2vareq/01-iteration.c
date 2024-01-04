//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main() {
    int i, j;
    int size = 5;

    for (i = 0; i < size; ++i) {
        j = i;

       __goblint_check(i == j); //SUCESS
     }

    return 0;
}

//This test case checks whether the value of variable i is always equal to the value of variable j within the loop.
