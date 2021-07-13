// SKIP PARAM: --sets solver td3 --enable ana.int.interval --enable exp.partition-arrays.enabled  --set ana.activated "['base','threadid','threadflag','expRelation','octApron','mallocWrapper']"
int main(void) {
    f1();
}

int f1() {
    int one;
    int two;

    int x;

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
