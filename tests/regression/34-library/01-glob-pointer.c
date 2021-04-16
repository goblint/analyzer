//PARAM: --enable ana.library --enable allfuns --sets ana.activated[+] mallocWrapperTypeBased
#include <assert.h>

int g;

int f(int *x){
    int a = 1;
    if(x==&g){
        a = 0;
    }
    assert(a == 1); // UNKNOWN!
    assert(a == 0); // UNKNOWN!
}
