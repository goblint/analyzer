// SKIP PARAM: --set solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','apron','escape']" --set ana.base.privatization none --set ana.apron.privatization top
extern int __VERIFIER_nondet_int();

void change(int *p) {
    // Check that `a` refers to the local of `change` and not to the local of `main`
    int a;
    (*p)++;
    a++;
    assert(a == 7); //UNKNOWN!
}

int g;
int main() {
    int c = __VERIFIER_nondet_int();
    int a = 5;
    int *p = &a;
    change(p);
    assert(a == 5); //FAIL
    assert(a - 6 == 0); // Apron used to find \bot here (!)
    return 0;
}
