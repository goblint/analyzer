//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

int* gptr;

// Mutex to ensure we don't get race warnings, but the UAF warnings we actually care about
pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;

void *t_other(void* p) {
    pthread_mutex_lock(&mtx);
    free(gptr); //WARN
    pthread_mutex_unlock(&mtx);
}

int main() {
    gptr = malloc(sizeof(int));
    *gptr = 42;

    pthread_t thread;
    pthread_create(&thread, NULL, t_other, NULL);

    pthread_mutex_lock(&mtx);
    *gptr = 43; //WARN
    free(gptr); //WARN
    pthread_mutex_unlock(&mtx);

    return 0;
}