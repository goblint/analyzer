// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set sem.int.signed_overflow assume_none

#include <pthread.h>
#include <assert.h>

unsigned int x, y, z;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void* T1_SL(void* arg){
    int i = 0, j = 0, k = 0;

    for(i = 0; i < x; i++) {
        for(j = i + 1; j < y; j++) {
            for(k = j; k < z; k++) {
                pthread_mutex_lock(&lock);
                assert(k > i);
                pthread_mutex_unlock(&lock);
            }
        }
    }
    return NULL;
}

void* T2_SL(void* arg){
    int i = 0, j = 0, k = 0;

    for(i = 0; i < x; i++) {
        for(j = i + 1; j < y; j++) {
            for(k = j; k < z; k++) {
                pthread_mutex_lock(&lock);
                assert(k > i);
                pthread_mutex_unlock(&lock);
            }
        }
    }
    return NULL;
}

int main(){
    int top1, top2, top3;
    x = top1;
    y = top2;
    z = top3;
    pthread_t t1;
    pthread_t t2;
    pthread_create(&t1, 0, T1_SL, 0);
    pthread_create(&t2, 0, T2_SL, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);
    return 0;
}
