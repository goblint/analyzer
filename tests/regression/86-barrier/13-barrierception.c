// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
// In this example, the barriers "deadlock"
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include<goblint.h>

int g;
int h;

pthread_barrier_t barrier;
pthread_barrier_t barrier2;
pthread_mutex_t mutex;

void* f1(void* ptr) {
    g = 2; //NORACE
    pthread_barrier_wait(&barrier);
    pthread_barrier_wait(&barrier2);

    return NULL;
}

int main(int argc, char const *argv[])
{
    int top;
    int i = 0;

    pthread_barrier_init(&barrier, NULL, 2);
    pthread_barrier_init(&barrier2, NULL, 2);

    pthread_t t1;
    pthread_create(&t1,NULL,f1,NULL);

    if(top) {
        pthread_barrier_wait(&barrier2);
        pthread_barrier_wait(&barrier);
        i = 2;
    }

    __goblint_check(i == 0);



    return 0;
}
