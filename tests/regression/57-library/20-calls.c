// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs
#include <stdio.h>
#include <assert.h>

int *foo(int *x){
    return x;
}

int *bar(int *x){
    foo(x);
    return x;
}

int *fooBar(int *x){
    foo(x);
    bar(x);
    return x;
}

int main(){
    int i = 3;
    foo(&i);
    bar(&i);
    int *p;
    p = foo(&i);
    int z = 1;
    if(p == &i){
        z = 0;
    }
    assert(z == 0); //UNKNOWN
    return 0;
}
