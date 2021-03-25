//PARAM: --enable allfuns --enable ana.library  --set ana.activated "['base','mallocWrapperTypeBased']"

#include <stdlib.h>

int main(){
    int *p = malloc(sizeof(int));
    int *p2 = malloc(sizeof(int));

    int z = 1;
    if(p == p2){
        z = 0;
    } else {
        z = 32;
    }
    // p and p2 should be represented by the same non-definite memory-block representation.
    // Therefore, the following assert should be unknown:
    assert(z == 1); // UNKNOWN
    return 0;
}
