// SKIP PARAM: --set ana.activated[+] apron
#include <goblint.h>

extern int __VERIFIER_nondet_int();

void change(int *p) {
    (*p)++;
}

int g;
int main() {
    int c = __VERIFIER_nondet_int();
    g = 3;
    __goblint_check(g != 3); // FAIL
    __goblint_check(g == 3);
    int a = 5;
    int *p = &a; // after this apron should put a to top because pointers are not tracked
    change(p);
    __goblint_check(a == 5); //FAIL
    __goblint_check(a - 6 == 0);
    return 0;
}
