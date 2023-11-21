//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak --set ana.activated[+] thread
#include <stdlib.h>
#include <pthread.h>

int *m1;

void *f2(void *arg) {
    m1 = malloc(sizeof(int));
    while (1);
    return NULL;
}

void *f1(void *arg) {
    pthread_t t2;
    pthread_create(&t2, NULL, f2, NULL);

    pthread_exit(NULL);
    return NULL;
}

int main(int argc, char const *argv[]) {
    pthread_t t1;
    pthread_create(&t1, NULL, f1, NULL);
    pthread_join(t1, NULL);

    return 0; // WARN
}