#include<stdlib.h>
#include<assert.h>
int main(void) {
    int *r = calloc(5,sizeof(int));
    //r[3] = 4;
    r[0] = 3;

    int z = r[1];

    assert(z == 0); 
}