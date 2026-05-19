// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include<goblint.h>

int g;

pthread_barrier_t barrier;


int main(int argc, char const *argv[])
{
    int top;
    int i = 0;

    pthread_barrier_init(&barrier, NULL, 2);

    if(top) {
        pthread_barrier_wait(&barrier);
        // Unreachable
        i = 1;
    }

    __goblint_check(i == 0);
    return 0;
}
