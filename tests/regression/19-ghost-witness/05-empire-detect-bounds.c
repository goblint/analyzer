// CRAM
// Dominik's empire thing example
#include<pthread.h>
#include<goblint.h>
extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int x;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;


void fun() {
    int y = x;
    while (y> 0) {
        y--;
    }

    pthread_mutex_lock(&mutex);
    x++;
    pthread_mutex_unlock(&mutex);
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

    pthread_mutex_lock(&mutex);
    x++;
    pthread_mutex_unlock(&mutex);

    pthread_join(thread, NULL);

    top = 8;
    top = 3;

    return 0;
}
