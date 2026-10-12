// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<goblint.h>

pthread_barrier_t barrier;

// Capacity two orders g; capacity three blocks both threads before the second access.
// Goblint loses the lower bound and reports a false race.
int g;

void* f1(void* ptr) {
    g = 2; // TODO NORACE
    pthread_barrier_wait(&barrier);
    return NULL;
}

int main(int argc, char const *argv[])
{
    unsigned int capacity = argc > 1 ? 3 : 2;
    pthread_barrier_init(&barrier, NULL, capacity);

    pthread_t t1;
    pthread_create(&t1,NULL,f1,NULL);

    pthread_barrier_wait(&barrier);
    g = 3; // TODO NORACE

    return 0;
}
