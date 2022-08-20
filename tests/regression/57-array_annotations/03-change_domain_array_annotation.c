// PARAM: --set ana.base.arrays.unrolling-factor 5 --enable annotation.array --enable ana.int.interval


int main(void);

void test(int x[] __attribute__((goblint_array_domain("unroll")))){

    x[0] = 0;
    x[1] = 1;
    x[2] = 2;
    x[3] = 3;

    assert(x[0] == 0);
    assert(x[1] == 1);
    assert(x[2] == 2);
    assert(x[3] == 3);

}

void partitioned(int* a __attribute__((goblint_array_domain("partitioned")))){

    int i = 0;

    while(i < 42) {
        a[i] = 4;
        i++;
    }

    assert(a[3] == 4);

    i = 0;
    while(i<10) {
        a[i] = -1;
        i++;
    }

    assert(a[3] == -1);
    assert(a[i] == 4);
    assert(a[i+5] == 4);

}

int test2(void)
{

}



int main(void)
{
    int z[4] __attribute__((goblint_array_domain("trivial")));

    test(z);

    assert(z[0] == 0); // UNKNOWN

    int a[42] __attribute__((goblint_array_domain("unroll")));

    a[0] = 0;
    a[4] = 4;
    a[10] = 10;
    a[11] = 11;
    
    assert(a[4] == 4);
    assert(a[10] == 10); //UNKNOWN

    partitioned(a);
}
