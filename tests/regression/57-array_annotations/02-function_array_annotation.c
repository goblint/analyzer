// PARAM: --set ana.base.arrays.unrolling-factor 4 --enable annotation.array


int main(void);

void test(int x[4] __attribute__((goblint_array_domain("unroll")))){
    x[0] = 0;
    x[1] = 1;
    x[2] = 2;
    x[3] = 3;
    
    assert(x[0] == 0);
    assert(x[1] == 1);
    assert(x[2] == 2);
    assert(x[3] == 3);

}

int main(void)
{
    int z[4] __attribute__((goblint_array_domain("unroll"))) ;

    test(z);

    assert(z[0] == 0);

    assert(z[0] == 0);
    assert(z[1] == 1);
    assert(z[2] == 2);
    assert(z[3] == 3);
}
