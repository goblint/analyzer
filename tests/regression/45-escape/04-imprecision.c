#include <pthread.h>
#include <goblint.h>

int* gptr;

void *foo(void* p){
    *gptr = 17;
    return NULL;
}

// The asserts about y would hold in the concrete, as y actually never escapes the current thread, but we are currently not precise enough to account for this

int main(){
    int x = 0;
    int y = 0;
    gptr = &y;
    gptr = &x;
    __goblint_check(x==0);
    pthread_t thread;
    pthread_create(&thread, NULL, foo, NULL);
    sleep(3);
    __goblint_check(x == 0); // UNKNOWN!
    __goblint_check(y == 0); //TODO
    y = 5;
    __goblint_check(y == 5); //TODO
    pthread_join(thread, NULL);
    return 0;
}
