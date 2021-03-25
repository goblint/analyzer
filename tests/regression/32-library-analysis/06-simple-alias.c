//PARAM: --enable allfuns --enable ana.library  --set ana.activated "['base','mallocWrapperTypeBased']"

#include<assert.h>

int main(){
    int x = 3;
    int* p = &x;
    int* q = &x;

    int z = 1;
    if(p == q){
        z = 0;
    }
    assert(z == 0);
    return 0;
}
