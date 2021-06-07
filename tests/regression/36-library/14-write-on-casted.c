//PARAM: --enable ana.library --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs
#include <stdlib.h>

int foo(void *ptr){
    int *iptr = (int*) ptr;
    int top = rand();
    if(top){
        *iptr = 34;
    }
    return 0;
}

int main(){
    int x = 12;
    int *ptr;
    ptr = &x;
    foo(ptr);
    assert(x == 12); // UNKNOWN!
    return 0;
}
