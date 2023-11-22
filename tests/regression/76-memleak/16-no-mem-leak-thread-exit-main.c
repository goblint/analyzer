//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak --set ana.activated[+] thread
#include <stdlib.h>
#include <pthread.h>

int *m1;

void *f1(void *arg) {
    m1 = malloc(sizeof(int));
    while (1);
    return NULL;
}

int main(int argc, char const *argv[]) {
    pthread_t t1;
    pthread_create(&t1, NULL, f1, NULL);

    // A pthread_exit called in main will wait for other threads to finish
    // Therefore, no memory leak here
    pthread_exit(NULL); // NOWARN

    return 0; // NOWARN (unreachable)
}