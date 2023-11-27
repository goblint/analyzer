// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>

int main(int argc, char const *argv[]) {
    int i = 42;
    int *ptr = &i;

    *ptr = 5;//NOWARN
    *(ptr + 10) = 55;//WARN

    return 0;
}
