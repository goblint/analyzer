// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs
#include <stdlib.h>

int *g = NULL;
int z = 32;

int foo(int **ptr){
    int** p;
    p = &g;
    *p = malloc(sizeof(int*));
    int x = 0;
    if(*ptr == *p){
        x = 1;
        g = &z;
    }
    assert(x == 0); // UNKNOWN!
    return 0;
}

int main(){
    int **ptr = &g;
    foo(ptr);
    return 0;
}
