// PARAM: --set ana.activated[+] 'maylocks'
#include<pthread.h>
#include<stdio.h>
#include<unistd.h>
#include <assert.h>

pthread_mutex_t mut[8];

void* f1(void* ptr) {
    int top;
    int x = 2;
    if(top) {
        x = 3;
    }


    pthread_mutex_lock(&mut[x]);
    pthread_mutex_lock(&mut[3]); //WARN
    pthread_mutex_unlock(&mut[3]);
    return NULL;
}


int main(int argc, char const *argv[])
{
    pthread_t t1;
    pthread_t t2;

    pthread_create(&t1,NULL,f1,NULL);
    pthread_join(t1, NULL);

    pthread_mutex_lock(&mut); //NOWARN
    pthread_mutex_unlock(&mut);

    return 0;
}
