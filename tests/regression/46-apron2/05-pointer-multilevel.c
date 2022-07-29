// SKIP PARAM: --set solver td3 --set ana.activated[+] apron --set ana.apron.privatization top
#include <assert.h>

extern int __VERIFIER_nondet_int();

void otherchange(int* ptr) {
    (*ptr)++;
}

void change(int *p) {
    (*p)++;
    otherchange(p);
    int* ptr = &p;
    __goblint_check(*p ==7);
}

int g;
int main() {
    int c = __VERIFIER_nondet_int();
    g = 3;
    __goblint_check(g != 3); // FAIL
    __goblint_check(g == 3);
    int a = 5;
    int *p = &a;
    change(p);
    __goblint_check(a == 5); //FAIL
    __goblint_check(a - 7 == 0);
    return 0;
}
