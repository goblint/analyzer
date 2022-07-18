// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins  --enable ana.apron.threshold_widening

#include <pthread.h>
#include <assert.h>

unsigned int a, b, c;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void* T1_SL5(void* arg){
    while(1) {
        pthread_mutex_lock(&lock);
        assert(a != b); //TODO   requires disjunctions
        pthread_mutex_unlock(&lock);
    }
    return NULL;
}

void* T2_SL5(void* arg){
    while(1) {
        pthread_mutex_lock(&lock);
        int temp = a;
        a = b;
        b = c;
        c = temp;
        pthread_mutex_unlock(&lock);
    }
    return NULL;
}

void* T3_SL5(void* arg){
    while(1) {
        pthread_mutex_lock(&lock);
        int temp = a;
        a = b;
        b = c;
        c = temp;
        pthread_mutex_unlock(&lock);
    }
    return NULL;
}

int main(){
    a = 1;
    b = 2;
    c = 3;
    pthread_t t1;
    pthread_t t2;
    pthread_t t3;
    pthread_create(&t1, 0, T1_SL5, 0);
    pthread_create(&t2, 0, T2_SL5, 0);
    pthread_create(&t3, 0, T3_SL5, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);
    pthread_join(t3, 0);
    return 0;
}
