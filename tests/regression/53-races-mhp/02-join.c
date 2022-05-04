//PARAM: --set ana.activated[+] mhp --set ana.activated[+] threadJoins
#include <pthread.h>
#include <assert.h>

int g;
int g2;

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t n = PTHREAD_MUTEX_INITIALIZER;

void* fun0(void* arg) {
    pthread_mutex_lock(&m);
    int x = g2;
    pthread_mutex_unlock(&m);
}

void* fun(void* arg) {
    pthread_mutex_lock(&m);
    if(g < 100) {
        g++;
    };
    pthread_mutex_unlock(&m);

    pthread_mutex_lock(&n);
    int x = g2;
    pthread_mutex_unlock(&n);
}

int main() {
    pthread_t t1;
    pthread_t t0;
    pthread_create(&t0, 0, fun0, 0);

    g++; //NORACE

    pthread_create(&t1, 0, fun, 0);

    pthread_mutex_lock(&m);
    if(g < 100) {
        g++;
    };
    pthread_mutex_unlock(&m);

    pthread_join(t1, 0);

    g++; //NORACE

    return 0;
}
