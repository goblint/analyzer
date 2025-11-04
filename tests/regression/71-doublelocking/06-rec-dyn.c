// PARAM: --set ana.activated[+] 'maylocks' --set ana.activated[+] 'pthreadMutexType'
#define _GNU_SOURCE
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include <assert.h>

int g;

void* f1(void* ptr) {
    pthread_mutex_t* mut = (pthread_mutex_t*) ptr;

    pthread_mutex_lock(mut); //NOWARN
    pthread_mutex_lock(mut); //NOWARN
    pthread_mutex_unlock(mut);
    pthread_mutex_unlock(mut);
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


    pthread_mutex_lock(&mut); //NOWARN
    pthread_mutex_lock(&mut); //NOWARN
    pthread_mutex_unlock(&mut);
    pthread_mutex_unlock(&mut);

    pthread_join(t1, NULL);


    return 0;
}
