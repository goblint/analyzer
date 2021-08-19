// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

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
