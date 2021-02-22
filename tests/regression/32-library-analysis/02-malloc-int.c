//PARAM: --enable allfuns --enable ana.library  --set ana.activated "['base','mallocWrapperTypeBased']"
#include <stdlib.h>

int f(int *x){
    return *x;
}

int main(){
    double z = 1.0;
    void *v = malloc(sizeof(char)*100);
    int *x = malloc(sizeof(int));

    if(v==x){
        z = 0;
    }

    asser(z);
    return 2;
}
