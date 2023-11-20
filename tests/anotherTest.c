#include <pthread.h>
#include <stdio.h>

int shared_data = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *increment(void *arg) {
    for (int i = 0; i < 100000; ++i) {
        pthread_mutex_lock(&mutex);
        shared_data++;
        pthread_mutex_unlock(&mutex);
    }
    return NULL;
}

void *decrement(void *arg) {
    for (int i = 0; i < 100000; ++i) {
        pthread_mutex_lock(&mutex);
        shared_data--;
        pthread_mutex_unlock(&mutex);
    }
    return NULL;
}

int main() {
    pthread_t thread1, thread2;

    pthread_create(&thread1, NULL, increment, NULL);
    pthread_create(&thread2, NULL, decrement, NULL);

    pthread_join(thread1, NULL);
    pthread_join(thread2, NULL);

    printf("Final shared_data value: %d\n", shared_data);

    return 0;
}

//two threads modify a shared variable (shared_data) using a mutex for synchronization.

//a race condition on the shared_data varaible due to simultaneous reads and writes from differenet threads without proper synchronization.