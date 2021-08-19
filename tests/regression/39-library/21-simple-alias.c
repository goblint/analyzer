// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

#include<assert.h>

int main(){
    int x = 3;
    int* p = &x;
    int* q = &x;

    int z = 1;
    if(p == q){
        z = 0;
    }
    assert(z == 0);
    return 0;
}
