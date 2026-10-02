// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<goblint.h>

pthread_barrier_t barrier;

// Capacity is initialized through a pointer; later changes cannot alter the barrier.

void* f1(void* ptr) {
    pthread_barrier_wait(&barrier);
    return NULL;
}

int main(int argc, char const *argv[])
{
    int top;
    int i = 0;

    unsigned int capacity;
    unsigned int* count = &capacity;
    *count = 2;

    pthread_barrier_init(&barrier, NULL, capacity);
    *count = 3; // The initialized barrier still needs only two waiters.

    pthread_t t1;
    pthread_create(&t1,NULL,f1,NULL);

    if(top) {
        pthread_barrier_wait(&barrier);
        i = 1;
    }

    __goblint_check(i == 0); //UNKNOWN!

    return 0;
}
