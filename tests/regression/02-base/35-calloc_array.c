// PARAM: --set ana.int.interval true --enable exp.partition-arrays.enabled

#include<stdlib.h>
#include<assert.h>

int main(void) {
    int *r = calloc(5,sizeof(int));

    assert(r[0] == 0); 
    
    r[0] = 3;

    assert(r[0] == 3); //UNKNOWN 

    int z = r[1];

    assert(z == 0); //UNKNOWN 
    assert(z != 365);
}