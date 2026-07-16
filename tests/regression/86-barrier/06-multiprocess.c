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

    pthread_barrierattr_t barattr;
    pthread_barrierattr_setpshared(&barattr, PTHREAD_PROCESS_SHARED);

    pthread_barrier_init(&barrier, &barattr, 2);

    fork();
    pthread_t t1;

    if(top) {
        pthread_barrier_wait(&barrier);
        // Reachable if both processes go into this branch
        i = 1;
    }


    __goblint_check(i == 0); //UNKNOWN!


    return 0;
}
