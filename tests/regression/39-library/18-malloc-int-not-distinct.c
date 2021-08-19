// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

#include <stdlib.h>


int main(){
    int *x = malloc(sizeof(int));
    int *y = malloc(sizeof(int));

    int z = 1;
    if(x==y){
        z = 0;
    }

    assert(z == 1); // UNKNOWN!

    return 2;
}
