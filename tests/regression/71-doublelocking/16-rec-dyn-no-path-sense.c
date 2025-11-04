// PARAM: --set ana.activated[+] 'pthreadMutexType' --set ana.path_sens[-] 'mutex'
// Test that multiplicity also works when path-sensitivity is disabled.
#define _GNU_SOURCE
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include <assert.h>

int g;

void* f1(void* ptr) {
    pthread_mutex_t* mut = (pthread_mutex_t*) ptr;
    int top;


    pthread_mutex_lock(mut);

    if(top) {
        pthread_mutex_lock(mut);
    }

    pthread_mutex_unlock(mut);
    g = 8; //RACE!


    return NULL;
}


int main(int argc, char const *argv[])
{
    pthread_t t1;
    pthread_mutex_t mut;

    pthread_mutexattr_t attr;
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(&mut, &attr);


    pthread_create(&t1,NULL,f1,&mut);


    pthread_mutex_lock(&mut);
    g = 9; // RACE!
    pthread_mutex_unlock(&mut);

    pthread_join(t1, NULL);


    return 0;
}
