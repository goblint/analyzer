// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<goblint.h>

pthread_barrier_t barrier;

// Depending on argc, capacity is one (main passes) or two (main blocks).

int main(int argc, char const *argv[])
{
    int top;
    int i = 0;

    unsigned int capacity = argc > 1 ? 2 : 1;

    pthread_barrier_init(&barrier, NULL, capacity);

    if(top) {
        pthread_barrier_wait(&barrier);
        i = 1;
    }

    __goblint_check(i == 0); //UNKNOWN!

    return 0;
}
