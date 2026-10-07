// CRAM
// Like 05, but x is a global pointer to a dynamically allocated integer.
#include<pthread.h>
#include<stdlib.h>
#include<goblint.h>
extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int *x;


void fun() {
    int y = *x;
    while (y> 0) {
        y--;
    }

    (*x)++;
}

int main(void) {
    int z;
    int top;

    x = malloc(sizeof(int));

    if(top) { *x = 10000; }

    pthread_t thread;
    pthread_create(&thread, NULL, (void*)fun, NULL);

    while(z != 0) {
        z--;
    }

    (*x)++;

    pthread_join(thread, NULL);

    top = 8;
    top = 3;

    return 0;
}
