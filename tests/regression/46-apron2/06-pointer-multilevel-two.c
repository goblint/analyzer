// SKIP PARAM: --set solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','apron','escape']" --set ana.base.privatization none --set ana.apron.privatization top
extern int __VERIFIER_nondet_int();

void change(int *p,int i) {
    (*p)++;
    int* ptr = &p;
    assert(*p == 6);
}

int g;
int main() {
    int c = __VERIFIER_nondet_int();
    g = 3;
    assert(g != 3); // FAIL
    assert(g == 3);
    int a = 5;
    int *p = &a;
    change(p, a);
    assert(a == 5); //FAIL
    assert(a - 6 == 0); // Apron used to find \bot here (!)
    return 0;
}
