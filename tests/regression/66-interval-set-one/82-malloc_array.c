// PARAM: --set ana.int.interval_set true --set ana.base.arrays.domain partitioned
#include<stdlib.h>
#include <goblint.h>

int main(void) {
    int *r = malloc(5 * sizeof(int));

    r[3] = 2;

    __goblint_check(r[4] == 2);
    /* Here we only test our implementation. Concretely, accessing the uninitialised r[4] is undefined behavior.
    In our implementation we keep the whole memory allocated by malloc as one Blob and the whole Blob contains 2 after it was assigned to r[3].
    This is more useful than keeping the Blob unknown. */
}
