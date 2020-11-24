// PARAM: --set ana.int.interval true --enable exp.partition-arrays.enabled
#include<stdlib.h>
#include<assert.h>

int main(void) {
    int *r = malloc(5 * sizeof(int));

    r[3] = 2;

    assert(r[4] == 2); 
    /* Here we only test our implementation. Concretely, accessing the uninitialised r[4] is undefined behavior.
    In our implementation we keep the whole memory allocated by malloc as one Blob and the whole Blob contains 2 after it was assigned to r[3].
    This is more useful than keeping the Blob unknown. */
}