#include <pthread.h>
#include <assert.h>

int* gptr;

void *foo(void* p){
    *gptr = 17;
    return NULL;
}

int main(){
    int x = 0;
    gptr = &x;
    pthread_t thread;
    pthread_create(&thread, NULL, foo, NULL);
    sleep(3);
    assert(x == 0); // UNKNOWN!
    pthread_join(thread, NULL);
    return 0;
}
