// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs
#include <stdlib.h>

typedef struct int_wrapper {
    int value;
} wrapper;

typedef struct ctr {
    int x;
    int *ptr;
    wrapper *w;
} container;

int manipulate(container *cont,  wrapper *w, int *value){
    int top = rand();
    if(top){
        *value = 4;
    }
    return 0;
}

int main(){
    wrapper wra = {7};
    container ctr = {4, NULL, NULL};
    assert(wra.value == 7);
    manipulate(&ctr, &wra, &wra.value);
    assert(wra.value == 7); //UNKNOWN!
    return 0;
}

int manipulate2(container *cont,  wrapper *w, int *value){
    int top = rand();
    if(top){
        cont->w->value = 23;
    }
    return 0;
}

int main2(){
    wrapper wra = {7};
    container ctr = {4, NULL, &wra};
    assert(ctr.w->value == 7);
    assert(ctr.x == 4);
    manipulate2(&ctr, &wra, &wra.value);
    assert(ctr.w->value == 7); //UNKNOWN!
    assert(ctr.x == 4);
    return 0;
}

int manipulate3(container *cont,  wrapper *w, int *value){
    int top = rand();
    if(top){
        *value = 23;
    }
    return 0;
}

int main3(){
    wrapper wra = {7};
    container ctr = {4, NULL, &wra};
    assert(ctr.w->value == 7);
    assert(ctr.x == 4);
    manipulate3(&ctr, &wra, &ctr.x);
    assert(ctr.w->value == 7); //UNKNOWN
    assert(ctr.x == 4); //UNKNOWN!
    return 0;
}
