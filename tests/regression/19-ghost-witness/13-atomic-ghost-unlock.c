// SKIP CRAM
// Check that ghost instrumentation does not release an atomic block from the
// program when instrumenting a statement inside it.
#include<pthread.h>
#include<goblint.h>
extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int g;
int seen;

void fun() {
    __VERIFIER_atomic_begin();
    seen = g;
    __goblint_check(seen != 10);
    __VERIFIER_atomic_end();
}

int main(void) {
    pthread_t thread;

    pthread_create(&thread, NULL, (void*)fun, NULL);

    __VERIFIER_atomic_begin();
    g = 10;
    g = 2;
    __VERIFIER_atomic_end();

    pthread_join(thread, NULL);

    return 0;
}
