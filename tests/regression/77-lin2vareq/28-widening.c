// SKIP PARAM: --set ana.activated[+] lin2vareq

#include <stdio.h>

int main() {
    int x = 0;
    int y = 1;

    for (int i = 0; i < 1000; i++) {
        x += i;
        y += x;
    }

    __goblint_check(x == 499500); //UNKNOWN
    __goblint_check(y == 499501); //UNKNOWN

    return 0;
}
