// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs
#include <stdlib.h>

int f(int *x){
    return *x;
}

int main(){
    void *tmp = malloc(sizeof(char) * 100);
    int top;
    f((int*) tmp);
    return 2;
}
