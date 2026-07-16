//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak --set ana.activated[+] thread
#include <stdlib.h>
#include <pthread.h>


void *f2(void *arg) {
    int* m1 = malloc(sizeof(int));
    free(m1);
    return NULL;
}

// We check here that the analysis can distinguish between thread returns and normal returns

void startf2(pthread_t* t){
    pthread_create(t, NULL, f2, NULL);
    return; //NOWARN
}

void *f1(void *arg) {
    pthread_t t2;
    startf2(&t2);
    pthread_join(t2, NULL);
    return NULL; // NOWARN
}

int main(int argc, char const *argv[]) {
    pthread_t t1;
    pthread_create(&t1, NULL, f1, NULL);
    pthread_join(t1, NULL);

    return 0; // NOWARN
}