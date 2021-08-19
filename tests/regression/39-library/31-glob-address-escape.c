// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

#include <assert.h>
int *pptr;

int foo(int *x){
    pptr = x;
    return 0;
}

int bar(){
    int x = 12;
    int y = 33;

    int *yptr = &y;
    int *xptr = &x;

    pptr = yptr;
    assert(pptr == yptr); // UNKNOWN

    foo(xptr);

    assert(pptr == yptr); // UNKNOWN
    assert(pptr == xptr); // UNKNOWN

    return 0;
}

int main(){
    bar();
}
