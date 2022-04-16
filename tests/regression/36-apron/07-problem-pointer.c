// SKIP PARAM: --set solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','apron']" --set ana.base.privatization none --set ana.apron.privatization dummy
extern int __VERIFIER_nondet_int();

void change(int *p) {
    (*p)++;
}

int g;
int main() {
    int c = __VERIFIER_nondet_int();
    g = 3; // Globals are not tracked by apron for now
    assert(g != 3); // FAIL
    assert(g == 3);
    int a = 5;
    int *p = &a; // after this apron should put a to top because pointers are not tracked
    change(p);
    assert(a == 5); //FAIL
    assert(a - 6 == 0);
    return 0;
}
