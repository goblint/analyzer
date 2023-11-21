//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak --set ana.activated[+] thread
#include <stdlib.h>
#include <pthread.h>

int *m1;

void *f1(void *arg) {
    m1 = malloc(sizeof(int));
    while (1);
}

int main(int argc, char const *argv[]) {
    pthread_t t1;
    pthread_create(&t1, NULL, f1, NULL);

    pthread_exit(NULL);

    pthread_join(t1, NULL);

    // A pthread_join called in main will wait for other threads to finish
    // Therefore, no memory leak here
    return 0; // NOWARN
}