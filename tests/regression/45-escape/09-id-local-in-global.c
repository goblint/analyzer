#include <pthread.h>
#include <goblint.h>

int* gptr;

void *foo(void* p){
    *gptr = 17;
    return NULL;
}

int* id(int* x) {
    return x;
}

int main(){
    int x = 0;
    gptr = id(&x);
    __goblint_check(x==0);
    pthread_t thread;
    pthread_create(&thread, NULL, foo, NULL);
    sleep(3);
    __goblint_check(x == 0); // UNKNOWN!
    pthread_join(thread, NULL);
    return 0;
}
