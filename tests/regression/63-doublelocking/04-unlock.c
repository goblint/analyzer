#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include <assert.h>

int g;

pthread_mutex_t mut = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mut2 = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cond = PTHREAD_COND_INITIALIZER;

void* f1(void* ptr) {
    int top;

    pthread_mutex_lock(&mut);
    pthread_mutex_unlock(&mut2); //WARN
    return NULL;
}


int main(int argc, char const *argv[])
{
    pthread_t t1;
    pthread_t t2;

    pthread_create(&t1,NULL,f1,NULL);
    pthread_join(t1, NULL);

    pthread_mutex_lock(&mut);
    pthread_mutex_unlock(&mut); //NOWARN

    pthread_cond_wait(&cond,&mut); //WARN

    return 0;
}
