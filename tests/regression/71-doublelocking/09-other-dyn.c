// PARAM: --set ana.activated[+] 'maylocks' --set ana.activated[+] 'pthreadMutexType'
#define _GNU_SOURCE
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include <assert.h>

int g;

void* f1(void* ptr) {
    pthread_mutex_t* mut = (pthread_mutex_t*) ptr;

    pthread_mutex_lock(mut);
    pthread_mutex_lock(mut); //WARN

    return NULL;
}


void* f2(void* ptr) {
    pthread_mutex_t* mut = (pthread_mutex_t*) ptr;

    pthread_mutex_lock(mut);
    pthread_mutex_unlock(mut);

    // To check that this is now actually removed from the may lockset
    return NULL; //NOWARN
}



int main(int argc, char const *argv[])
{
    pthread_t t1;
    pthread_t t2;
    pthread_mutex_t mut;

    pthread_mutexattr_t attr;
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_NORMAL);
    pthread_mutex_init(&mut, &attr);


    pthread_create(&t1,NULL,f1,&mut);


    pthread_mutex_lock(&mut);
    pthread_mutex_lock(&mut); //WARN


    pthread_join(t1, NULL);

    pthread_create(&t2,NULL,f2,&mut);

    return 0;
}
