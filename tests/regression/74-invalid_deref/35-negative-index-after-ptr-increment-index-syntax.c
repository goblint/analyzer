// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>

int main() {
    int *b = malloc(2 * sizeof(int));

    *b++ = 0; //NOWARN

    if (b[-2]) //WARN
        return 1;

    free(b - 1);
    return 0;
}
