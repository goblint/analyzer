// PARAM: --enable ana.int.interval --enable ana.base.partition-arrays.enabled
#include <assert.h>
#include <stdlib.h>
#include <limits.h>
#include <stdio.h>
#include <stddef.h>

// Test to check whether partitioned arrays can have an index expression evaluating to values largers than the max value of int64

#define LENGTH (LONG_MAX - 600)
#define STOP (LENGTH - 1)

int main(){
    // Check that ptrdiff_t is at least as big as long, so we can index arrays with non-negative longs
    assert(sizeof(ptrdiff_t) >= sizeof(long));

    char *arr = calloc(LENGTH, sizeof(char));
    if(arr == NULL){
        printf("Could not allocate array, exiting.\n");
        return 1;
    }

    for(unsigned long i = 0; i < STOP; i++){
        arr[i] = 1;
    }

    // arr[0] ... arr[STOP - 1] should be 1, the others equal to 0
    assert(arr[0] == 1); // UNKNOWN
    assert(arr[INT_MAX + 1l] == 1); //UNKNOWN

    // j is the smallest index where checking it used to yield an unsound value
    // long j = ((long) INT_MAX) * INT_MAX * 2 + INT_MAX - 1;
    long j = LONG_MAX - 6442450943;
    assert(0 < j);
    assert(j < STOP);

    // This check is imprecise, but not unsound
    assert(arr[j - 1] == 1); //UNKNOWN

    // These two asserts used to fail somehow
    assert(arr[j] == 1); //UNKNOWN
    assert(arr[STOP - 1] == 1); //UNKNOWN

    assert(arr[STOP] == 0); //UNKNOWN
    assert(arr[LENGTH - 1] == 0); //UNKNOWN
    return 0;
}
