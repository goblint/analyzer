// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --sets exp.partition-arrays.keep-expr "last" --set ana.activated "['base','expRelation']"
int main(void) {
    int a[4];
    int b[4];

    a[0] = 42;
    a[1] = 42;
    a[2] = 42;
    a[3] = 42;

    assert(a[0] == 42);
    assert(a[1] == 42);
    assert(a[2] == 42);
    assert(a[3] == 42);

    int *ptr = &b;
    *ptr = 1; ptr++;
    *ptr = 1; ptr++;
    *ptr = 1; ptr++;
    *ptr = 1; ptr++;

    assert(b[0] == 1);
    assert(b[1] == 1);
    assert(b[2] == 1);
    assert(b[3] == 1);
}
