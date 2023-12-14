//SKIP PARAM: --set ana.activated[+] lin2vareq
// from https://dl.acm.org/doi/10.1145/2049706.2049710

#include <stdio.h>

int main() {
    int x1 = 5, x2 = 10, x3 = 15, x4, x5, x6, x7, x8, x9, x10, x11, x12;

    x4 = 3 * x2 + 5;
    x5 = 3 * x3 + 15;
    x6 = x3 + 3;
    x7 = x3 + 2;
    x8 = 7 * x3 + 15;
    x9 = 0;
    x10 = 2 * x9 + 2;
    x11 = 2 * x1 - 3;
    x12 = 4 * x1 - 5;

    __goblint_check(x4 == 3 * x2 + 5); //SUCCESS
    __goblint_check(x5 == 3 * x3 + 15); //SUCCESS
    __goblint_check(x7 == x6 - 1); //SUCCESS
    __goblint_check(x10 == 2 * x9 + 2); //SUCCESS
    __goblint_check(x12 == 2 * x11 + 1); //SUCCESS

    return 0;
}