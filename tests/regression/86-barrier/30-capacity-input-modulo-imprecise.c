// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<goblint.h>

pthread_barrier_t barrier;

// The remainder makes capacity two or three, so the sole waiter always blocks.
// Goblint loses this lower bound under the default integer configuration.

int main(int argc, char const *argv[])
{
    int top;
    int i = 0;

    unsigned int capacity = (unsigned int)argc % 2 + 2;

    pthread_barrier_init(&barrier, NULL, capacity);

    if(top) {
        pthread_barrier_wait(&barrier);
        i = 1;
    }

    __goblint_check(i == 0); // TODO

    return 0;
}
