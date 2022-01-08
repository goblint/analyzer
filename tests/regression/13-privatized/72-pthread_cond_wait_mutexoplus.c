// PARAM: --sets ana.base.privatization mutex-oplus
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include <assert.h>

int g;

pthread_mutex_t mut = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cond = PTHREAD_COND_INITIALIZER;

void* f1(void* ptr) {
    pthread_mutex_lock(&mut);
    g = 1;
    pthread_cond_wait(&cond,&mut);
    assert(g == 0); //UNKNOWN!
    assert(g != 1); //UNKNOWN!
    printf("g is %i", g);
    g = 0;
    pthread_mutex_unlock(&mut);
    return NULL;
}

void* f2(void* ptr) {
    pthread_mutex_lock(&mut);
    assert(g == 0); //UNKNOWN!
    g = 0;
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
