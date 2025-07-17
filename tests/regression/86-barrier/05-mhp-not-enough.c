// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include<goblint.h>

int g;

pthread_barrier_t barrier;


void* f1(void* ptr) {
    pthread_barrier_wait(&barrier);

    // This should not be reached as either main calls wait or the other thread is created
    // -> In the concrete, there never are three threads calling wait

    return NULL;
}

int main(int argc, char const *argv[])
{
    int top;
    int i = 0;

    pthread_barrier_init(&barrier, NULL, 3);

    pthread_t t1;
    pthread_create(&t1,NULL,f1,NULL);

    if(top) {
        pthread_barrier_wait(&barrier);
        // Unreachable
        i = 1;
    } else {
        pthread_t t2;
        pthread_create(&t2,NULL,f1,NULL);
    }


    __goblint_check(i == 0);


    return 0;
}
