// PARAM: --set ana.activated[+] 'pthreadMutexType'
// Check we don't have a stack overflow because of tracking multiplicities
#define _GNU_SOURCE
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include <assert.h>

int g;

void f2(pthread_mutex_t* mut) {
    int top1, top2;
    pthread_mutex_lock(mut);
    if(top1 == top2) {
        // This would cause the number of contexts to explode
        f2(mut);
    }
    pthread_mutex_unlock(mut);
}

void* f1(void* ptr) {
    pthread_mutex_t* mut = (pthread_mutex_t*) ptr;
    f2(mut);
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
    pthread_join(t1, NULL);
    return 0;
}
