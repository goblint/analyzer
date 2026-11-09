#include <pthread.h>

int i;

void *t_fun(void *arg) {
    // atomic functions don't race with each other:
    __atomic_store_n(&i, 12, __ATOMIC_RELAXED); // NORACE
    __atomic_fetch_add(&i, 1, __ATOMIC_RELAXED); // NORACE
    __atomic_add_fetch(&i, 1, __ATOMIC_RELAXED); // NORACE
    __sync_fetch_and_add(&i, 1); // NORACE
    return NULL;
}

int main(void) {
    pthread_t id, id2;
    pthread_create(&id, NULL, t_fun, NULL);
    pthread_create(&id2, NULL, t_fun, NULL);
    pthread_join(id, NULL);
    pthread_join(id2, NULL);
    return 0;
}
