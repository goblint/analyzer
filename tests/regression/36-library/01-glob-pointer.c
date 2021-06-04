//PARAM: --enable ana.library --sets ana.activated[+] mallocWrapperTypeBased --sets ana.activated[-] mallocWrapper --sets ana.activated[+] typecasts
#include <assert.h>

int g;

int f(int *x){
    int a = 1;
    int *g_ptr = &g;
    if(x==&g){
        a = 0;
    }
    assert(a == 1); // UNKNOWN!
    assert(a == 0); // UNKNOWN!
}
