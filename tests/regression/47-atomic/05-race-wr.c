#include <pthread.h>
#include <stdio.h>

// race between non-atomic write and atomic read

int i;

void *thread1(void *arg) {
    i = 5; // RACE!
    return NULL;
}
void *thread2(void *arg) {
    printf("%d\n", __atomic_load_n(&i, __ATOMIC_RELAXED)); // RACE!
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
