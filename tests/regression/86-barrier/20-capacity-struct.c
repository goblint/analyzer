// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<goblint.h>

pthread_barrier_t barrier;

// The struct field holds two, ordering the accesses on opposite sides of the barrier.
int g;
int h;

void* f1(void* ptr) {
    g = 2; //NORACE
    h = 3; //RACE
    pthread_barrier_wait(&barrier);
    return NULL;
}

int main(int argc, char const *argv[])
{
    struct { unsigned int capacity; } config;
    config.capacity = 2;
    pthread_barrier_init(&barrier, NULL, config.capacity);

    pthread_t t1;
    pthread_create(&t1,NULL,f1,NULL);

    h = 5; //RACE
    pthread_barrier_wait(&barrier);
    g = 3; //NORACE

    return 0;
}
