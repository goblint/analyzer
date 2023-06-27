// PARAM: --set ana.activated[+] memOutOfBounds
#include <stdlib.h>

int main(int argc, char const *argv[]) {
    char *ptr = malloc(5 * sizeof(char));

    *ptr = 'a';//NOWARN
    *(ptr + 1) = 'b';//NOWARN
    *(ptr + 10) = 'c';//WARN

    free(ptr);

    return 0;
}
