//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak
#include <stdlib.h>
#include <pthread.h>

int *g;
int *m1;
int *m2;

void *f1(void *arg) {
    int top;

    // Thread t1 leaks m0 here
    exit(2); //WARN
}

int main(int argc, char const *argv[]) {
    pthread_t t1;
    pthread_create(&t1, NULL, f1, NULL);

    int* m0 = malloc(sizeof(int));
    free(m0);

    // main thread is not leaking anything
    return 0;
}
