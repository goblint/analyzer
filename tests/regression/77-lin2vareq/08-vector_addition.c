//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main() {
    int vector1[] = {1, 2, 3};
    int vector2[] = {4, 5, 6};
    int result[3];


    for (int i = 0; i < 3; ++i) {
        result[i] = vector1[i] + vector2[i];
    }

    __goblint_check(result[0] == 5 && result [1] == 7 && result [2] == 9); //SUCCESS
    
    return 0;
}
