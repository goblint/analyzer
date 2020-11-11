// PARAM: --sets solver td3 --enable ana.int.interval --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation','octagon']"
int main(void) {
    f1();
}

int f1() {
    int one;
    int two;

    int x;

    one = two;

    // We no longer compute with "top" in the interval domain,
    // leading to a loss of precision here.
    // Thus the three asserts are here marked with "UNKNOWN".
    assert(one - two == 0); // UNKNOWN
    x = f2(one,two);
    assert(one - two == 0); // UNKNOWN
    assert(x == 48);
}

int f2(int a, int b) {
    assert(a-b == 0); // UNKNOWN

    return 48;
}
