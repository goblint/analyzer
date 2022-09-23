// PARAM: --set ana.base.arrays.domain partitioned  --set ana.base.partition-arrays.keep-expr "last" --enable annotation.int.enabled --set ana.int.refinement fixpoint
#include <assert.h>

int main(void) {
    example1();
    example2();
    return 0;
}

void example1(void) __attribute__((goblint_precision("no-def_exc","interval")));
void example2(void) __attribute__((goblint_precision("no-def_exc","interval")));


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

        __goblint_check(a[i][0] == 42);
        __goblint_check(a[i][9] == 42);
        __goblint_check(a[3][9] == 42); // UNKNOWN

        i++;
    }

    __goblint_check(a[0][0] == 42);
    __goblint_check(a[2][5] == 42);
    __goblint_check(a[8][9] == 42);
    __goblint_check(a[3][7] == 42);
    __goblint_check(a[9][9] == 42); // UNKNOWN
    __goblint_check(a[9][2] == 42); // UNKNOWN
}

// Combines backwards- and forwards-iteration
void example2(void) {
    int array[10][10];
    int i = 9;

    while(i >= 0) {
        int j =0;

        while(j < 10) {
            array[i][j] = 4711;
            __goblint_check(array[i-1][j+1] == 4711); //UNKNOWN
            j++;
        }

        i--;
    }

    __goblint_check(array[2][3] == 4711);
    __goblint_check(array[0][9] == 4711);
    __goblint_check(array[8][5] == 4711);
    __goblint_check(array[2][1] == 4711);
    __goblint_check(array[0][0] == 4711);
    __goblint_check(array[7][5] == 4711);
}
