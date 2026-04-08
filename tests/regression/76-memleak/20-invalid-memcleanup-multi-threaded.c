//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak --set ana.activated[+] thread
#include <stdlib.h>
#include <pthread.h>

int *g;
int *m1;

void *f1(void *arg) {
    m1 = malloc(sizeof(int));
    // Thread t1 leaks m1 here
    pthread_exit(NULL); //WARN
}

void *f2(void *arg) {
    int *m2;
    m2 = malloc(sizeof(int));
    free(m2); // No leak for thread t2, since it calls free before exiting
    pthread_exit(NULL); //NOWARN
}

int main(int argc, char const *argv[]) {
    g = malloc(sizeof(int));
    pthread_t t1;
    pthread_create(&t1, NULL, f1, NULL);

    pthread_t t2;
    pthread_create(&t2, NULL, f2, NULL);

    free(g);

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    // main thread is not leaking anything
    return 0; //NOWARN
}
