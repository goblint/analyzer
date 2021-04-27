//PARAM: --enable ana.library --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased --sets ana.activated[-] mallocWrapper --sets ana.activated[+] writtenLvals

#include <assert.h>
int foo(int *p, long *l){
    *l = 4;
    *p = 3;
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
    assert(p==3); // UNKNOWN
    assert(l==4);
    return 0;
}
