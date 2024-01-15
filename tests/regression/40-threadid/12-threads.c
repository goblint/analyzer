
#include <goblint.h>
#include <pthread.h>

/* Slab sizing definitions. */
#define POWER_SMALLEST 1
#define POWER_LARGEST 16 /* actual cap is 255 */
#define LARGEST_ID POWER_LARGEST
/* Locks for cache LRU operations */
pthread_mutex_t lru_locks[POWER_LARGEST];
static pthread_t item_crawler_tid;

int myglobal;

void memcached_thread_init(int nthreads, void *arg) {
    int i;
    for (i = 0; i < POWER_LARGEST; i++) {
        pthread_mutex_init(&lru_locks[i], NULL);
    }
}

void *item_crawler_thread(void *arg) {
    int i, r1, r2;
    for (i = POWER_SMALLEST; i < LARGEST_ID; i++) {
        pthread_mutex_lock(&lru_locks[i]);
        myglobal = myglobal + 1; // RACE!
        if (r1) {
            pthread_mutex_unlock(&lru_locks[i]);
            continue;
        }
        // if (r2) {
        //     int x = 0;
        // }
    }
    return NULL;
}

int main(void) {
    int i;
    int num_threads = rand();
    memcached_thread_init(num_threads, NULL);
    pthread_create(&item_crawler_tid, NULL, item_crawler_thread, NULL);
    for (i = POWER_SMALLEST; i < LARGEST_ID; i++) {
        pthread_mutex_lock(&lru_locks[i]);
        myglobal = myglobal + 1; // RACE!
        pthread_mutex_unlock(&lru_locks[i]);
    }
    pthread_join(&item_crawler_tid, NULL);
    return 0;
}