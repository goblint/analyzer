extern int __VERIFIER_nondet_int();

// SKIP PARAM: --set solver td3 --enable ana.int.interval --enable exp.partition-arrays.enabled  --set ana.activated "['base','threadid','threadflag','expRelation','apron','mallocWrapper']" --set exp.privatization none --set exp.apron.privatization dummy
int main(void) {
    f1();
}

int f1() {
    int one = __VERIFIER_nondet_int();
    int two = __VERIFIER_nondet_int();

    int x = __VERIFIER_nondet_int();

    one = two;

    assert(one - two == 0);
    assert(one == two);
    x = f2(one,two);
    assert(one - two == 0);
    assert(one == two);
    assert(x == 48);
}

int f2(int a, int b) {
    assert(a-b == 0);
    assert(a == b);

    return 48;
}
