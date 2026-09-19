// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<goblint.h>

pthread_barrier_t barrier;
pthread_barrier_t barrier2;

// Reuse one local count to initialize barriers with different fixed capacities.
// Each barrier must retain its own count, including after the local changes again.

int g;
int h;

void* f1(void* ptr) {
    g = 2; //RACE
    pthread_barrier_wait(&barrier);
    h = 2; //NORACE
    pthread_barrier_wait(&barrier2);
    return NULL;
}

int main(int argc, char const *argv[])
{
    unsigned int capacity = 3;
    capacity -= 2;

    pthread_barrier_init(&barrier, NULL, capacity);
    ++capacity;
    pthread_barrier_init(&barrier2, NULL, capacity);
    ++capacity;

    pthread_t t1;
    pthread_create(&t1,NULL,f1,NULL);

    // Capacity one allows main to pass without waiting for the worker.
    pthread_barrier_wait(&barrier);
    g = 3; //RACE

    // Capacity two orders h, even though the local count is now three.
    pthread_barrier_wait(&barrier2);
    h = 3; //NORACE

    return 0;
}
