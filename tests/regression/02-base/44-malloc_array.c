// PARAM: --set ana.int.interval true --enable exp.partition-arrays.enabled
#include<stdlib.h>
#include<assert.h>

int main(void) {
    int *r = malloc(5 * sizeof(int));

    r[3] = 2;

    assert(r[4] == 2);
}