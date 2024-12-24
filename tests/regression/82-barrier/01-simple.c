// PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>

int g;

pthread_barrier_t barrier;

void* f1(void* ptr) {
    return NULL;
}

void* f2(void* ptr) {
    return NULL;
}

int main(int argc, char const *argv[])
{
    pthread_barrier_init(&barrier, NULL, 2);
    pthread_barrier_wait(&barrier);
    
    pthread_t t1;
    pthread_t t2;

    pthread_create(&t1,NULL,f1,NULL);
    sleep(1);
    pthread_create(&t2,NULL,f2,NULL);

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    return 0;
}
