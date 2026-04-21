// CRAM
// Dominik's empire thing example
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
