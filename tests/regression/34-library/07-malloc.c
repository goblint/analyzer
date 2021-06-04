//PARAM: --enable ana.library --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased --sets ana.activated[-] mallocWrapper --sets ana.activated[+] writtenLvals --sets ana.activated[+] typecasts

#include <stdlib.h>
#include <assert.h>

void foo(int **ptr){
    int *p = malloc(sizeof(int));
    *p = 23;
    *ptr = p;
}

int main(){
    int x = 3;
    int *ptr = &x;
    int **pptr = & ptr;
    foo(pptr);
    assert(x == 3);
    assert(**pptr == 3); // UNKNOWN!
    return 0;
}
