// PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned
#include <goblint.h>
#include <stdlib.h>
#include <limits.h>
#include <stdio.h>
#include <stddef.h>

// Test to check whether partitioned arrays can have an index expression evaluating to values larger than the max value of int64

#define LENGTH (LONG_MAX - 600)
#define STOP (LENGTH - 1)

int main(){
    // Check that ptrdiff_t is at least as big as long, so we can index arrays with non-negative longs
    __goblint_check(sizeof(ptrdiff_t) >= sizeof(long));

    char *arr = calloc(LENGTH, sizeof(char));
    if(arr == NULL){
        printf("Could not allocate array, exiting.\n");
        return 1;
    }

    for(unsigned long i = 0; i < STOP; i++){
        arr[i] = 1;
    }

    // arr[0] ... arr[STOP - 1] should be 1, the others equal to 0
    __goblint_check(arr[0] == 1); // UNKNOWN
    __goblint_check(arr[INT_MAX + 1l] == 1); //UNKNOWN

    // j is the smallest index where checking it used to yield an unsound value
    // long j = ((long) INT_MAX) * INT_MAX * 2 + INT_MAX - 1;
    long j = LONG_MAX - 6442450943;
    __goblint_check(0 < j);
    __goblint_check(j < STOP);

    // This check is imprecise, but not unsound
    __goblint_check(arr[j - 1] == 1); //UNKNOWN

    // These two asserts used to fail somehow
    __goblint_check(arr[j] == 1); //UNKNOWN
    __goblint_check(arr[STOP - 1] == 1); //UNKNOWN

    __goblint_check(arr[STOP] == 0); //UNKNOWN
    __goblint_check(arr[LENGTH - 1] == 0); //UNKNOWN
    return 0;
}
