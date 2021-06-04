//PARAM: --enable ana.library --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased --sets ana.activated[-] mallocWrapper --sets ana.activated[+] writtenLvals --sets ana.activated[+] typecasts
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
