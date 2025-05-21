#include <goblint.h>
#include <pthread.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

int (*fp)(void) = NULL;

int bad() {
    return -1;
}

int good() {
    return 1;
}

void* consumer(void *arg) {
    int res = 0;
    pthread_mutex_lock(&mutex);
    if (fp != NULL) {
        res = fp();
    }
    pthread_mutex_unlock(&mutex);
    __goblint_check(res >= 0); // UNKNOWN before, success after
    res = 0;
    // change absorbed
    return NULL;
}

void* producer(void *arg) {
    int res = 0;
    pthread_mutex_lock(&mutex);
    fp = bad;
    pthread_mutex_unlock(&mutex);
    return NULL;
}

int main() {
    pthread_t id1 = NULL, id2 = NULL;
    pthread_create(&id1, NULL, consumer, NULL);
    pthread_create(&id2, NULL, producer, NULL);
    return 0;
}