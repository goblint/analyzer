//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main() {
    int vector1[] = {1, 2, 3};
    int vector2[] = {1, 2, 3};

    __goblint_check(vector1[0] == vector2[0] && vector1[1] == vector2[1] && vector1[2] == vector2[2]);

    return 0;
}

