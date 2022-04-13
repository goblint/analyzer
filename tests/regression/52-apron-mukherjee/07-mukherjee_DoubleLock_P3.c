// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins

#include <pthread.h>
#include <assert.h>

int count;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void* T1_DLP3(void* arg){
    pthread_mutex_lock(&lock);
    count++;
    count--;
    pthread_mutex_unlock(&lock);

    pthread_mutex_lock(&lock);
    count--;
    count++;
    pthread_mutex_unlock(&lock);
    return NULL;
}

void* T2_DLP3(void* arg){
    pthread_mutex_lock(&lock);
    assert(count >= -1);
    pthread_mutex_unlock(&lock);
    return NULL;
}

int main(){
    count = 0;

    pthread_t t1;
    pthread_t t2;
    pthread_create(&t1, 0, T1_DLP3, 0);
    pthread_create(&t2, 0, T2_DLP3, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);
    return 0;
}
