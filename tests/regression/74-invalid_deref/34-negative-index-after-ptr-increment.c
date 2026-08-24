// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>

int main() {
    int *b = malloc(2 * sizeof(int));
    int x;

    *b++ = 0; //NOWARN

    x = *(b - 2); //WARN

    free(b - 1);
    return x;
}
