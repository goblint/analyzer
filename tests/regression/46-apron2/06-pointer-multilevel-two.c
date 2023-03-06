// SKIP PARAM: --set ana.activated[+] apron --set ana.relation.privatization top
#include <goblint.h>

extern int __VERIFIER_nondet_int();

void change(int *p,int i) {
    (*p)++;
    int* ptr = &p;
    __goblint_check(*p == 6);
}

int g;
int main() {
    int c = __VERIFIER_nondet_int();
    g = 3;
    __goblint_check(g != 3); // FAIL
    __goblint_check(g == 3);
    int a = 5;
    int *p = &a;
    change(p, a);
    __goblint_check(a == 5); //FAIL
    __goblint_check(a - 6 == 0); // Apron used to find \bot here (!)
    return 0;
}
