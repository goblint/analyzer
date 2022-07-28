// SKIP PARAM: --set solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','apron','escape']" --set ana.base.privatization none --set ana.apron.privatization top
#include <assert.h>

extern int __VERIFIER_nondet_int();

void otherchange(int* ptr) {
    (*ptr)++;
}

void change(int *p) {
    (*p)++;
    otherchange(p);
    int* ptr = &p;
    assert(*p ==7);
}

int g;
int main() {
    int c = __VERIFIER_nondet_int();
    g = 3;
    assert(g != 3); // FAIL
    assert(g == 3);
    int a = 5;
    int *p = &a;
    change(p);
    assert(a == 5); //FAIL
    assert(a - 7 == 0);
    return 0;
}
