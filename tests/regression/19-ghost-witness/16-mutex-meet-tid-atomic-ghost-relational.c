// CRAM
// Ghost-aware atomic privatization preserves linear relations between the phase and protected values.
#include<pthread.h>
#include<goblint.h>

extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int x;
int y;

void fun() {
    __VERIFIER_atomic_begin();
    x = 1;
    y = 3;
    __VERIFIER_atomic_end();

    __VERIFIER_atomic_begin();
    x = 2;
    y = 6;
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
