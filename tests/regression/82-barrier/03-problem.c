// PARAM: --set ana.activated[+] 'pthreadBarriers'
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>

int g;

pthread_barrier_t barrier;


void* f1(void* ptr) {
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
    
    if(top) {
        pthread_barrier_wait(&barrier);
        // Live!
        i = 1;
    }

    __goblint_check(i == 0); //UNKNOWN!


    return 0;
}
