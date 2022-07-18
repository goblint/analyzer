// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <assert.h>

static int a = 0;
static int b = 0;

pthread_mutex_t mut_lock = PTHREAD_MUTEX_INITIALIZER;

void *iSet_1(void *param);
void *iSet_2(void *param);
void *iCheck_1(void *param);
void *iCheck_2(void *param);

int main(int argc, char *argv[]) {
    int i, err;

    pthread_t t1;
    pthread_t t2;
    pthread_t t3;
    pthread_t t4;

    pthread_create(&t1, NULL, &iSet_1, NULL);
    pthread_create(&t2, NULL, &iSet_2, NULL);
    pthread_create(&t3, NULL, &iCheck_1, NULL);
    pthread_create(&t4, NULL, &iCheck_2, NULL);

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);
    pthread_join(t3, NULL);
    pthread_join(t4, NULL);


    return 0;
}

void *iSet_1(void *param) {
    pthread_mutex_lock(&mut_lock);
    a = 1;
    b = -1;
    pthread_mutex_unlock(&mut_lock);
    return NULL;
}

void *iSet_2(void *param) {
    pthread_mutex_lock(&mut_lock);
    a = 1;
    b = -1;
    pthread_mutex_unlock(&mut_lock);
    return NULL;
}

void *iCheck_1(void *param) {
    pthread_mutex_lock(&mut_lock);
    assert(a + b == 0);
    pthread_mutex_unlock(&mut_lock);

    return NULL;
}

void *iCheck_2(void *param) {
    pthread_mutex_lock(&mut_lock);
    assert(a + b == 0);
    pthread_mutex_unlock(&mut_lock);

    return NULL;
}
