//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

int* gptr;

// Mutex to ensure we don't get race warnings, but the UAF warnings we actually care about
pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;

void *t_use(void* p) {
    pthread_mutex_lock(&mtx);
    *gptr = 0; //NOWARN
    pthread_mutex_unlock(&mtx);
}

int main() {
    gptr = malloc(sizeof(int));
    *gptr = 42;

    pthread_t using_thread;
    pthread_create(&using_thread, NULL, t_use, NULL);
    
    // Join using_thread before freeing gptr in the main thread
    pthread_join(using_thread, NULL);

    pthread_mutex_lock(&mtx);
    *gptr = 43; //NOWARN
    free(gptr); //WARN
    pthread_mutex_unlock(&mtx);

    return 0;
}