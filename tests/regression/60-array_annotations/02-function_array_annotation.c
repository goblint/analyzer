// PARAM: --set ana.base.arrays.unrolling-factor 4 --enable annotation.goblint_array_domain
#include <goblint.h>

int main(void);

void test(int x[4] __attribute__((goblint_array_domain("unroll")))){
    x[0] = 0;
    x[1] = 1;
    x[2] = 2;
    x[3] = 3;

    __goblint_check(x[0] == 0);
    __goblint_check(x[1] == 1);
    __goblint_check(x[2] == 2);
    __goblint_check(x[3] == 3);

}

int main(void)
{
    int z[4] __attribute__((goblint_array_domain("unroll"))) ;

    test(z);

    __goblint_check(z[0] == 0);

    __goblint_check(z[0] == 0);
    __goblint_check(z[1] == 1);
    __goblint_check(z[2] == 2);
    __goblint_check(z[3] == 3);
}
