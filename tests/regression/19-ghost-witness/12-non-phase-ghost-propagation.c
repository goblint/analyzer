// CRAM
// Check that non-phase ghost values are carried over between phases and
// joined with branch-local updates in the new phase.
#include<pthread.h>
#include<stdlib.h>
#include<goblint.h>

int *x;

void fun() {
    int y = *x;
    while (y > 0) {
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
        *published = 10;
    }

    return 0;
}
