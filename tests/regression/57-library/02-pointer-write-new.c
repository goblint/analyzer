// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs
#include <assert.h>

int g = 2;

int f(int *x){
    g = 2;
    *x = 5;
    assert(g == 2); // UNKNOWN!
    return g;
}
