// NOMAC PARAM: --set ana.activated[+] 'pthreadBarriers'
// This example exploits information on one barrier to become more precise for the other. Inception!
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include<goblint.h>

int g;
int h;

pthread_barrier_t barrier1;
pthread_barrier_t barrier2;
pthread_mutex_t mutex;

void* f1(void* ptr) {
    pthread_barrier_wait(&barrier1);
    pthread_barrier_wait(&barrier2);

    return NULL;
}

int main(int argc, char const *argv[])
{
    int top;
    int i = 0;

    pthread_barrier_init(&barrier1, NULL, 2);
    pthread_barrier_init(&barrier2, NULL, 2);

    pthread_t t1;
    pthread_create(&t1,NULL,f1,NULL);

    if(top == 2) {
        pthread_barrier_wait(&barrier1);
    } else if (top == 3) {
        // Here, we cleverly exploit the additional MHP information, that for f1 to call wait on barrier2,
        // it must have seen a call to wait by main on barrier1.
        pthread_barrier_wait(&barrier2);
        i = 2;
    }

    __goblint_check(i == 0);

    return 0;
}
