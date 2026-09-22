// CRAM
// Ghost phases recover atomically protected values that plain mutex-meet-tid-atomic cannot validate.
#include<pthread.h>
#include<goblint.h>

extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int x;

void fun() {
    __VERIFIER_atomic_begin();
    x = 1;
    __VERIFIER_atomic_end();

    __VERIFIER_atomic_begin();
    x = 2;
    __VERIFIER_atomic_end();
}

int main(void) {
    pthread_t thread;
    pthread_create(&thread, NULL, (void*)fun, NULL);

    __VERIFIER_atomic_begin();
    
    __VERIFIER_atomic_end();
    pthread_join(thread, NULL);

    return 0;
}
