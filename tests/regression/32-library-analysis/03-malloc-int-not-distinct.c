//PARAM: --enable allfuns --enable ana.library  --set ana.activated "['base','mallocWrapperTypeBased']"
#include <stdlib.h>


int main(){
    int *x = malloc(sizeof(int));
    int *y = malloc(sizeof(int));

    int z = 1;
    if(x==y){
        z = 0;
    }

    assert(z == 1); // UNKNOWN!

    return 2;
}
