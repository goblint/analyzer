// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

typedef struct foo{
    long *ptr;
    long x;
} bar;


#include <stdlib.h>
#include <assert.h>
int main(){
    int *p = malloc(sizeof(int));
    int *p2 = malloc(sizeof(int));

    bar *p3 = malloc(sizeof(bar));

    *p = 2;
    *p2 = 3;
    (*p3).x = 3;
    *(p3->ptr) = 2;

    int z = 1;
    int *ptr = &z;
    *ptr = 2;

    if(p == p2){
        z = 0;
    } else {
        z = 32;
    }
    // p and p2 should be represented by the same non-definite memory-block representation.
    // Therefore, the following assert should be unknown:
    assert(z == 0); // UNKNOWN
    return 0;
}
