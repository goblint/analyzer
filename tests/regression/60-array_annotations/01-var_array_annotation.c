// PARAM: --set ana.base.arrays.unrolling-factor 4 --enable annotation.array


int main(void);

int main(void)
{
    int x[4] __attribute__((goblint_array_domain("unroll")));

    x[0] = 0;
    x[1] = 1;
    x[2] = 2;
    x[3] = 3;

    __goblint_check(x[0] == 0);
    __goblint_check(x[1] == 1);
    __goblint_check(x[2] == 2);
    __goblint_check(x[3] == 3);

}
