// PARAM: --set ana.activated[+] 'pthreadSignals'
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include <assert.h>

int g;

pthread_mutex_t mut = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cond = PTHREAD_COND_INITIALIZER;

void* f1(void* ptr) {
    int top;
    pthread_mutex_lock(&mut);
    g = 1;
    if(top) {
        pthread_cond_wait(&cond,&mut); //NOWARN
    }
    pthread_mutex_unlock(&mut);
    return NULL;
}

void* f2(void* ptr) {
    pthread_mutex_lock(&mut);
    pthread_cond_signal(&cond);
    pthread_mutex_unlock(&mut);
    return NULL;
}

int main(int argc, char const *argv[])
{
    pthread_t t1;
    pthread_t t2;

    pthread_create(&t1,NULL,f1,NULL);
    sleep(1);
    pthread_create(&t2,NULL,f2,NULL);

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    return 0;
}
