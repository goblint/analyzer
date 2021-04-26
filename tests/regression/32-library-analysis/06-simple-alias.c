//PARAM: --enable ana.library --sets ana.activated[+] mallocWrapperTypeBased --sets ana.activated[-] mallocWrapper

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
