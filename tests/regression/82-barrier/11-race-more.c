// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>

int g;

pthread_barrier_t barrier;
pthread_mutex_t mutex;

void* f1(void* ptr) {
    pthread_mutex_lock(&mutex);
    g = 4711; //NORACE
    pthread_mutex_unlock(&mutex);
    pthread_barrier_wait(&barrier);

    return NULL;
}

int main(int argc, char const *argv[])
{
    int top;
    int i = 0;
    
    pthread_barrier_init(&barrier, NULL, 4);

    pthread_t t1, t2, t3;
    pthread_create(&t1,NULL,f1,NULL);
    pthread_create(&t2,NULL,f1,NULL);
    pthread_create(&t3,NULL,f1,NULL);

    pthread_barrier_wait(&barrier);

    g = 3; //NORACE

    return 0;
}
