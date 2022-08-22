// PARAM: --enable ana.int.interval --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 5
#include <assert.h>
int global;

int main(void)
{
    example1();
    example2();
    return 0;
}

void example1() {
    int a[20];
    a[4] = 4;
    a[6] = 6;
    a[10] = 10;
    __goblint_check(a[0] == 0); //UNKNOWN
    __goblint_check(a[4] == 4);
    __goblint_check(a[6] == 6); //UNKNOWN

    int i = 4;
    a[i] = 7;
    __goblint_check(a[4] == 7);
}

//array same length of factor
void example2() {
    int a[5];
    a[0] = 1;
    a[1] = 2;
    a[2] = 3;
    a[3] = 4;
    a[4] = 5;

    __goblint_check(a[0] == 1);
    __goblint_check(a[1] == 2);
    __goblint_check(a[2] == 3);
    __goblint_check(a[3] == 0); //FAIL
    __goblint_check(a[4] == 0); //FAIL
}

