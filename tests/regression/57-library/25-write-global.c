// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

#include<stdlib.h>

int *g = NULL;

int x;

int write_global(int **ptr){
    int* m = malloc(sizeof(int));
    g = m;
    return 0;
}

int main(){
    int *ptr = g;
    write_global(&g);

    ptr = g;
    return 0;
}
