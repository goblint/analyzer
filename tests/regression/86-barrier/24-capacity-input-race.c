// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<stdlib.h>
#include<goblint.h>

pthread_barrier_t barrier;

// Input one allows each thread to pass independently, so the barrier may not order g.
int g;

void* f1(void* ptr) {
    g = 2; //RACE
    pthread_barrier_wait(&barrier);
    return NULL;
}

int main(int argc, char const *argv[])
{
    if (argc < 2)
        return 0;
    int capacity = atoi(argv[1]);
    if (capacity < 1 || capacity > 2)
        return 0;
    pthread_barrier_init(&barrier, NULL, capacity);

    pthread_t t1;
    pthread_create(&t1,NULL,f1,NULL);

    pthread_barrier_wait(&barrier);
    g = 3; //RACE

    return 0;
}
