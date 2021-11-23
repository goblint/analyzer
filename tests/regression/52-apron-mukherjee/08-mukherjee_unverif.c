// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --enable ana.apron.threshold_widening

#include <pthread.h>
#include <assert.h>

unsigned int r = 0;
unsigned int s = 0;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void* thr1(void* arg){
    pthread_mutex_lock(&lock);
    r = r + 1;
    pthread_mutex_unlock(&lock);
    unsigned int l = 0;

    pthread_mutex_lock(&lock);
    if(r == 1){
        s = s + 1;
        l = l + 1;
        assert(s == l); // TODO
    }
    pthread_mutex_unlock(&lock);

    return 0;
}

void* thr2(void* arg){
    pthread_mutex_lock(&lock);
    r = r + 1;
    pthread_mutex_unlock(&lock);
    unsigned int l = 0;

    pthread_mutex_lock(&lock);
    if(r == 1){
        s = s + 1;
        l = l + 1;
        assert(s == l);  // TODO
    }
    pthread_mutex_unlock(&lock);

    return 0;
}

int main() {
    pthread_t t1;
    pthread_t t2;
    pthread_create(&t1, 0, thr1, 0);
    pthread_create(&t2, 0, thr2, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);
    return 0;
}
