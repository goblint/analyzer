// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs --enable ana.int.interval
#include <assert.h>
int foo(int *x){
    *x = 34;
    return 12;
}

int bar(int *ptr){
    int a = 0;
    *ptr = foo(&a);

    assert(a <= 34);
    assert(ptr == 12); //UNKNOWN
    return 0;
}


int main(){
    int a = 0;
    int r = 0;

    r = foo(&a);

    assert(a <= 34);
    assert(r == 12);
    return 0;
}
