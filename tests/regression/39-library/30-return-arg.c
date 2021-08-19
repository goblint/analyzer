// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

#include<assert.h>
#include<stdlib.h>

int z = 123;

int *foo(int *p, int* p2){
    int top, top2, top3;
    if(top)
        return &z;
    if(top2)
        return p2;
    if(top3)
        return NULL;
    return p;
}

int main(){
    int x = 10;
    int y = 12;
    int *p = &x;
    int *pp = &y;
    int *retP = foo(p, pp);

    assert(retP == NULL); // UNKNOWN!
    assert(retP == p); // UNKNOWN!
    assert(retP == pp); // UNKNOWN!
    assert(retP == &z); // UNKNOWN!

    int top;
    if(top)
        *retP = 2;

    assert(x == 10);  // UNKNOWN!
    return 0;
}
