// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation','octagon']"
int main(void) {
    f1();
}

int f1() {
    int one;
    int two;

    int x;

    one = two;

    assert(one - two == 0);
    x = f2(one,two);
    assert(one - two == 0);
    assert(x == 48);
}

int f2(int a, int b) {
    assert(a-b == 0);

    return 48;
}
