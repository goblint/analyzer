// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

#include <assert.h>
#include <stdarg.h>
#include <pthread.h>

void *foo(void *x){
    int* i = (int*) x;
    int top;
    if(top){
        *i = *i + 1;
    }
    return x;
}

int main(){
    pthread_t thread;
    int v = 10;
    assert(v == 10);
    void* ptr = &v;
    foo(ptr);
    pthread_create(&thread, NULL, foo, ptr);
    pthread_join(thread, NULL);
    assert(v == 10); //UNKNOWN!
    return 0;
}
