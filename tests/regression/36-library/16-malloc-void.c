//PARAM: --enable ana.library --sets ana.activated[+] mallocWrapperTypeBased --sets ana.activated[-] mallocWrapper --sets ana.activated[+] writtenLvals --sets ana.activated[+] typecasts
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
