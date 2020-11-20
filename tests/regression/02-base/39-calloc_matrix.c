// PARAM: --enable exp.partition-arrays.enabled

#include<stdlib.h>
#include<assert.h>

int main(void) {
    int (*r)[5] = calloc(2, sizeof(int[5]));
    r[0][1] = 3;
    int* z = &r[0][1];

    assert(*z == 3); //UNKNOWN
}