// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --enable ana.apron.threshold_widening

#include <pthread.h>
#include <assert.h>

int num = 1;

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void* T1_Sync01(void* arg) {
    pthread_mutex_lock(&m);
    while(num > 0)
        num++; // Here num overflows
    pthread_mutex_unlock(&m);
    return NULL;
}

void* T2_Sync01(void* arg) {
    pthread_mutex_lock(&m);
    while(num == 0)
        num--; // This never happens
    pthread_mutex_unlock(&m);
    return NULL;
}

int main() {
    pthread_t t1;
    pthread_t t2;
    pthread_create(&t1, 0, T1_Sync01, 0);
    pthread_create(&t2, 0, T2_Sync01, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);

    assert(num >= 0); // UNKNOWN!

    assert(num <= 1); // UNKNOWN!

    return 0;
}
