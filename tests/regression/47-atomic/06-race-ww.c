#include <pthread.h>

// race between atomic and non-atomic writes

int i;

void *thread1(void *arg) {
    __atomic_store_n(&i, 12, __ATOMIC_RELAXED); // RACE!
    return NULL;
}
void *thread2(void *arg) {
    i = 7; // RACE!
    return NULL;
}

int main(void) {
    pthread_t id, id2;
    pthread_create(&id, NULL, thread1, NULL);
    pthread_create(&id2, NULL, thread2, NULL);
    pthread_join(id, NULL);
    pthread_join(id2, NULL);
    return 0;
}
