// CRAM
#include <pthread.h>
#include <stdio.h>

int g1, g2;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t1(void *arg) {
    pthread_mutex_lock(&mutex1); // ghost_1 = 1
    pthread_mutex_lock(&mutex2); // ghost_2 = 1; ghost_3 = 1
    g1 = 5;
    g2 = g1 + 37;
    pthread_mutex_unlock(&mutex2); // ghost_2 = 0; ghost_3 = 0
    g1 = 0;
    pthread_mutex_unlock(&mutex1); // ghost_1 = 0
    return NULL;
}

void *t2(void *arg) {
    pthread_mutex_lock(&mutex2); // ghost_2 = 1; ghost_3 = 1
    g2 = g1 < 0 ? 41 : 42;
    pthread_mutex_unlock(&mutex2); // ghost_2 = 0; ghost_3 = 0

    pthread_mutex_lock(&mutex1); // ghost_1 = 1
    g1 = g2 - 42;
    pthread_mutex_unlock(&mutex1); // ghost_1 = 0
    return NULL;
}

int main(void) {
    pthread_t id1, id2;

    g1 = 0;
    g2 = 42;
    pthread_create(&id1, NULL, t1, NULL);
    pthread_create(&id2, NULL, t2, NULL);

    pthread_mutex_lock(&mutex1); // ghost_1 = 1
    g1 = 0;
    pthread_mutex_unlock(&mutex1); // ghost_1 = 0
    pthread_join (id1, NULL);
    pthread_join (id2, NULL);

  return 0;
}