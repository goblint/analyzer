// CRAM
// Variant of 05 without mutex protection, but with atomic x to avoid undefined behavior.
#include<pthread.h>
#include<goblint.h>
extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

_Atomic int x;


void fun() {
    int y = x;
    while (y> 0) {
        y--;
    }

    x = x + 1;
}

int main(void) {
    int z;
    int top;

    if(top) { x = 10000; }

    pthread_t thread;
    pthread_create(&thread, NULL, (void*)fun, NULL);

    while(z != 0) {
        z--;
    }

    x = x + 1;

    pthread_join(thread, NULL);

    top = 8;
    top = 3;

    return 0;
}
