// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins

#include <pthread.h>
#include <assert.h>

unsigned int x;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void* T1_Spin(void* arg){
    pthread_mutex_lock(&lock);
    x = 0;
    x = 1;

    assert(x >= 1);
    pthread_mutex_unlock(&lock);
    return NULL;
}

void* T2_Spin(void* arg){
    pthread_mutex_lock(&lock);
    x = 0;
    x = 1;

    assert(x >= 1);
    pthread_mutex_unlock(&lock);
    return NULL;
}

int main(){
    x = 1;
    pthread_t t1;
    pthread_t t2;
    pthread_create(&t1, 0, T1_Spin, 0);
    pthread_create(&t2, 0, T2_Spin, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);
    return 0;
}
