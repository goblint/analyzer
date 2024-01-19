// SKIP PARAM: --set ana.activated[+] lin2vareq

#include <stdio.h>

int compute(int x, int y) {
    int result = x * y - y;
    return result;
}

int main(void) {
    int a = 2, b = 3;
    int result = compute(a, b);

    __goblint_check(result == a * b - b); // SUCCESS
    __goblint_check(result + b == a * b); // SUCCESS

    return 0;
}
