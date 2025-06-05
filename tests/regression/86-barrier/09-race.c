// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include<goblint.h>

int g;
int h;

pthread_barrier_t barrier;
pthread_mutex_t mutex;

void* f1(void* ptr) {
    g = 2; //NORACE
    h = 3; //RACE
    pthread_barrier_wait(&barrier);

    return NULL;
}

int main(int argc, char const *argv[])
{
    int top;
    int i = 0;

    pthread_barrier_init(&barrier, NULL, 2);

    pthread_t t1;
    pthread_create(&t1,NULL,f1,NULL);

    h = 5; //RACE

    pthread_barrier_wait(&barrier);
    g = 3; //NORACE

    return 0;
}
