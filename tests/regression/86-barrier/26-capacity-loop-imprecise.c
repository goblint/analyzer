// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<goblint.h>

pthread_barrier_t barrier;

// The loop always computes two, so the barrier orders g.
// Goblint loses the exact capacity at the loop exit and reports a false race.
int g;
int h;

void* f1(void* ptr) {
    g = 2; // TODO NORACE
    h = 3; //RACE
    pthread_barrier_wait(&barrier);
    return NULL;
}

int main(int argc, char const *argv[])
{
    unsigned int capacity = 4;
    while (capacity > 2)
        --capacity;
    pthread_barrier_init(&barrier, NULL, capacity);

    pthread_t t1;
    pthread_create(&t1,NULL,f1,NULL);

    h = 5; //RACE
    pthread_barrier_wait(&barrier);
    g = 3; // TODO NORACE

    return 0;
}
