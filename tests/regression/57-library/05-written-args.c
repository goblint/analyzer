// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs


#include <assert.h>
int foo(int *p, long *l){
    int top;
    if(top){
        *l = 4;
        *p = 37;
    }
    return *p;
}

int bar(int *p, long *l){
    long ll = 1;
    return foo(p, &ll);
}

int main(){
    int p = 3;
    int q = 3;
    int *x = &q;
    long l = 4;
    *x = 323;
    bar(&p, &l);
    assert(p==3); // UNKNOWN!
    assert(l==4);
    return 0;
}
