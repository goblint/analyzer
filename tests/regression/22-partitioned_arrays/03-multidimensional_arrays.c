// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"
int main(void) {
    example1();
    example2();
    return 0;
}

// Two-dimensional array
void example1(void) {
    int a[10][10];
    int i=0;
    int j=0;

    while(i < 9) {

        j = 0;

        while(j < 10) {
            a[i][j] = 42;
            j++;
        }

        assert(a[i][0] == 42);
        assert(a[i][9] == 42);
        assert(a[3][9] == 42); // UNKNOWN

        i++;
    }

    assert(a[0][0] == 42);
    assert(a[2][5] == 42);
    assert(a[8][9] == 42);
    assert(a[3][7] == 42);
    assert(a[9][9] == 42); // UNKNOWN
    assert(a[9][2] == 42); // UNKNOWN
}

// Combines backwards- and forwards-iteration
void example2(void) {
    int array[10][10];
    int i = 9;

    while(i >= 0) {
        int j =0;

        while(j < 10) {
            array[i][j] = 4711;
            assert(array[i-1][j+1] == 4711); //UNKNOWN
            j++;
        }

        i--;
    }

    assert(array[2][3] == 4711);
    assert(array[0][9] == 4711);
    assert(array[8][5] == 4711);
    assert(array[2][1] == 4711);
    assert(array[0][0] == 4711);
    assert(array[7][5] == 4711);
}
