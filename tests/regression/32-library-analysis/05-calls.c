//PARAM: --enable ana.library --sets ana.activated[+] mallocWrapperTypeBased --sets ana.activated[-] mallocWrapper
#include <stdio.h>
#include <assert.h>

int *foo(int *x){
    return x;
}

int *bar(int *x){
    foo(x);
    return x;
}

int *fooBar(int *x){
    foo(x);
    bar(x);
    return x;
}

int main(){
    int i = 3;
    foor(&i);
    bar(&i);
    int *p;
    p = foo(&i);
    int z = 1;
    if(p == &i){
        z = 0;
    }
    assert(z == 0);
    return 0;
}
