// SKIP PARAM: --set ana.activated[+] apron --set ana.base.privatization none --set ana.apron.privatization top
#include <assert.h>

extern int __VERIFIER_nondet_int();

void change(int *p) {
    // Check that `a` refers to the local of `change` and not to the local of `main`
    int a;
    (*p)++;
    a++;
    __goblint_check(a == 7); //UNKNOWN!
}

int g;
int main() {
    int c = __VERIFIER_nondet_int();
    int a = 5;
    int *p = &a;
    change(p);
    __goblint_check(a == 5); //FAIL
    __goblint_check(a - 6 == 0); // Apron used to find \bot here (!)
    return 0;
}
