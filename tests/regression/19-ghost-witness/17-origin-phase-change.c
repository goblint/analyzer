// CRAM
// The worker has two alternative phase jumps from origin 0. Reaching phase 1
// must not enable the alternative target 3 as if it had originated at phase 1.
#include<pthread.h>
#include<goblint.h>

extern int __VERIFIER_nondet_int(void);

int x;

void *fun(void *arg) {
    if (__VERIFIER_nondet_int()) {
        x = 1;
    } else {
        x = 3;
    }
    return NULL;
}

int main(void) {
    pthread_t thread;
    pthread_create(&thread, NULL, fun, NULL);

    x = x;

    pthread_join(thread, NULL);

    return 0;
}
