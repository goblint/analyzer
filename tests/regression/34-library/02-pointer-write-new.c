//PARAM: --enable ana.library --enable allfuns --sets ana.activated[+] mallocWrapperTypeBased
#include <assert.h>

int g;

int f(int *x){
    g = 2;
    *x = 5;
    assert(g == 2); // UNKNOWN!
    return g;
}
