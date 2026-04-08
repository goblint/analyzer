//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak --disable warn.assert
#include <stdlib.h>
#include <pthread.h>
#include <assert.h>

void *f1(void *arg) {
    int top;
    assert(top); //WARN
}

int main(int argc, char const *argv[]) {
    pthread_t t1;
    pthread_create(&t1, NULL, f1, NULL);

    int* m0 = malloc(sizeof(int));
    free(m0);

    // main thread is not leaking anything
    return 0;
}
