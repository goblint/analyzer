//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main() {
    int x;
    int y = 5;

    x = y * y;

    __goblint_check(x == y * y); //SUCCESS

    return 0;
}
