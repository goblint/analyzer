// CRAM
// Check that the values of non-immediate variables are carried over
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
    int* published = malloc(sizeof(int));

    *published = 42;

    if(top) { *x = 10000; }

    pthread_t thread;
    pthread_create(&thread, NULL, (void*)fun, NULL);

    while(z != 0) {
        z--;
    }

    (*x)++;

    pthread_join(thread, NULL);

    if(top) {
        *published = 10; // As this conditional, the other value of published needs to be brought forward too
    }

    __goblint_check(*published == 10); //UNKNOWN!

    return 0;
}
