// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<goblint.h>

pthread_barrier_t barrier;

// The computed capacity is two, so main and the worker can pass.

void* f1(void* ptr) {
    pthread_barrier_wait(&barrier);
    return NULL;
}

int main(int argc, char const *argv[])
{
    int top;
    int i = 0;

    unsigned int workers = 1;
    unsigned int capacity = workers + 1;

    pthread_barrier_init(&barrier, NULL, capacity);

    pthread_t t1;
    pthread_create(&t1,NULL,f1,NULL);

    if(top) {
        pthread_barrier_wait(&barrier);
        i = 1;
    }

    __goblint_check(i == 0); //UNKNOWN!

    return 0;
}
