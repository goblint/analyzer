// SKIP PARAM: --set ana.activated[+] taintPartialContexts --set ana.activated[+] apron --set ana.ctx_insens[+] apron
#include <goblint.h>

int a_glob, b_glob;

int f1(int *aptr, int *bptr) {
    __goblint_check(*aptr - *bptr == 0); // UNKNOWN
}

int f2(int *aptr, int *bptr) {
    *aptr = 7;
}

int f3 (int *aptr, int *bptr) {
    *aptr = *bptr;
}

int main() {
    int a1, b1;
    int a2, b2;
    int a3, b3;

    //untainted
    f1(&a1, &b1);
    a1 = b1;
    a_glob = b_glob;
    f1(&a1, &b1);
    __goblint_check(a1 - b1 == 0);
    __goblint_check(a_glob - b_glob == 0);

    //tainted
    a2 = b2;
    f2(&a2, &b2);
    __goblint_check(a2 - b2 == 0);  //UNKNOWN!

    //add new
    f3(&a3, &b3);
    __goblint_check(a3 - b3 == 0);
}
