//PARAM: --enable ana.library --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs
#include <assert.h>

int g = 2;

int f(int *x){
    g = 2;
    *x = 5;
    assert(g == 2); // UNKNOWN!
    return g;
}
