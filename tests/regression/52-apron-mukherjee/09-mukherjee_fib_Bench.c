// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --enable ana.apron.threshold_widening

#include <pthread.h>
#include <assert.h>

int i, j, NUM;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void* W1_Fib_Bench_False_Unreach_Call(void* arg){
    for(int k = 0; k < NUM; k++) {
        pthread_mutex_lock(&lock);
        i += j;
        pthread_mutex_unlock(&lock);
    }
    return 0;
}

void* W2_Fib_Bench_False_Unreach_Call(void* arg){
    for(int k = 0; k < NUM; k++) {
        pthread_mutex_lock(&lock);
        j += i;
        pthread_mutex_unlock(&lock);
    }
    return 0;
}


int main() {
    i = 1;
    j = 1;

    NUM = 5;

    pthread_t t1;
    pthread_t t2;
    pthread_create(&t1, 0, W1_Fib_Bench_False_Unreach_Call, 0);
    pthread_create(&t2, 0, W2_Fib_Bench_False_Unreach_Call, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);

    assert(i < 144); //TODO
    assert(j < 144); //TODO

    return 0;
}
