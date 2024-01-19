// SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int x;
int y;

void setX(int a) {
    x = a;
}

void setY() {
    y = 2 * x + 3;
}

int main() {
    setX(5);
    setY();

    __goblint_check(y == 2 * x + 3); // SUCCESS

    return 0;
}
