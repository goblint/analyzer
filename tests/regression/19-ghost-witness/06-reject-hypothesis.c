// CRAM
// Based on 05, but witness hypothesis for ghost_a should be rejected.
#include<pthread.h>
#include<goblint.h>
extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int x;


void fun() {
    int y = x;
    while (y> 0) {
        y--;
    }

    x++;
}

int main(void) {
    int z;
    int top;

    if(top) { x = 10000; } else { x = 1; }

    pthread_t thread;
    pthread_create(&thread, NULL, (void*)fun, NULL);

    while(z != 0) {
        z--;
    }

    x++;

    pthread_join(thread, NULL);

    top = 8;
    top = 3;

    return 0;
}
