// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<goblint.h>

pthread_barrier_t barrier;

// The mutex permits only one waiter, which can still pass when input selects capacity one.

unsigned int get_capacity(int argc) {
    return argc > 1 ? 2 : 1;
}

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void* f1(void* ptr) {
    pthread_mutex_lock(&mutex);
    pthread_barrier_wait(&barrier);
    pthread_mutex_unlock(&mutex);
    return NULL;
}

int main(int argc, char const *argv[])
{
    int top;
    int i = 0;

    unsigned int capacity = get_capacity(argc);

    pthread_barrier_init(&barrier, NULL, capacity);

    pthread_t t1;
    pthread_create(&t1,NULL,f1,NULL);

    if(top) {
        pthread_mutex_lock(&mutex);
        pthread_barrier_wait(&barrier);
        pthread_mutex_unlock(&mutex);
        i = 1;
    }

    __goblint_check(i == 0); //UNKNOWN!

    return 0;
}
