// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins

#include <pthread.h>
#include <assert.h>

int x;
int y;

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void* T1(void* arg) {
    while(0 == 0) {
        pthread_mutex_lock(&m);
        if (x > 0) {
            x = x - 1;
            y = y - 1;
        }
        pthread_mutex_unlock(&m);
    }
    return NULL;
}

void* T2(void* arg) {
    while(0 == 0) {
        pthread_mutex_lock(&m);
        if (x < 10) {
            x = x + 1;
            y = y + 1;
        }
        pthread_mutex_unlock(&m);
    }
    return NULL;
}

int main() {
    pthread_t t1;
    pthread_t t2;
    pthread_create(&t1, 0, T1, 0);
    pthread_create(&t2, 0, T2, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);

    return 0;
}
