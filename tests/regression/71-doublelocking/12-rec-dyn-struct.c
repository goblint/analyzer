// PARAM: --set ana.activated[+] 'maylocks' --set ana.activated[+] 'pthreadMutexType'
// Like 06, but tests mutexattr survives joins
#define _GNU_SOURCE
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include <assert.h>

int g;
struct s {
    pthread_mutex_t mut;
};

typedef struct s s_t;


void* f1(void* ptr) {
    pthread_mutex_t* mut = &(((s_t*) ptr)->mut);

    pthread_mutex_lock(mut); //NOWARN
    pthread_mutex_lock(mut); //NOWARN
    pthread_mutex_unlock(mut);
    pthread_mutex_unlock(mut);
    return NULL;
}


int main(int argc, char const *argv[])
{
    pthread_t t1;
    s_t mut_str;

    pthread_mutexattr_t attr;

    if(argc == 2) {
        pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    } else {
        pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    }

    pthread_mutex_init(&mut_str.mut, &attr);


    pthread_create(&t1,NULL,f1,&mut_str);


    pthread_mutex_lock(&mut_str.mut); //NOWARN
    pthread_mutex_lock(&mut_str.mut); //NOWARN
    pthread_mutex_unlock(&mut_str.mut);
    pthread_mutex_unlock(&mut_str.mut);

    pthread_join(t1, NULL);


    return 0;
}
