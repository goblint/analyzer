// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins  --enable ana.apron.threshold_widening

#include <pthread.h>
#include <assert.h>

int SIZE = 2;
int OVERFLOW  = -1;
int top = 0;
int flag = 0;
int arr1;
int arr2;

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void* T1_Stack(void* arg) {
    int i;
    int j;
    pthread_mutex_lock(&m);
    j = SIZE;
    pthread_mutex_unlock(&m);
    for(i=0; i<j; i++) {
        pthread_mutex_lock(&m);
        assert(top < SIZE); //TODO

        if (top < SIZE) {
            top++;
        }
        flag = 1;
        pthread_mutex_unlock(&m);
    }
    return NULL;
}

void* T2_Stack(void* arg) {
    int i;
    int j;
    pthread_mutex_lock(&m);
    j = SIZE;
    pthread_mutex_unlock(&m);
    for(i=0; i<j; i++) {
    pthread_mutex_lock(&m);
    if (flag == 1) {
        if (top != 0)
            top--;
    }
    pthread_mutex_unlock(&m);
    }
    return NULL;
}

int main() {
    pthread_t t1;
    pthread_t t2;
    pthread_create(&t1, 0, T1_Stack, 0);
    pthread_create(&t2, 0, T2_Stack, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);
	return 0;
}
