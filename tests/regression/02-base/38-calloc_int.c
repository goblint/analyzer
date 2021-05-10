// PARAM: --set ana.int.interval true --enable exp.partition-arrays.enabled
#include<stdlib.h>
#include<assert.h>

int main(void) {
    int *r = calloc(1,sizeof(int));

    r[0] = 0;

    assert(r[0] != 5);
    assert(r[0] == 0);

    r[0] = 5;

    assert(r[0] == 5); //UNKNOWN
    assert(r[0] != 0); //UNKNOWN
    assert(r[0] != -10);
    assert(r[0] != 100);
}