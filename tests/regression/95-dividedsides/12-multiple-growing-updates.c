// PARAM: --enable ana.int.interval_set --enable solvers.td3.narrow-sides.enabled --enable solvers.td3.narrow-sides.stable
#include <pthread.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int glob = 0;
int glob2 = 0;

void *thread(void *) {    
    for(int i = 0; i < 10; i++) {
        pthread_mutex_lock(&mutex);
        glob = i;
        pthread_mutex_unlock(&mutex);
    }

    for(int i = -1; i > -10; i--) {
        pthread_mutex_lock(&mutex);
        glob = i;
        pthread_mutex_unlock(&mutex);
    }
}

int main(void)
{
    int id;
    pthread_create(&id, NULL, thread, NULL);
    pthread_mutex_lock(&mutex);
    __goblint_check(glob >= -10);
    __goblint_check(glob <= 10);
    pthread_mutex_unlock(&mutex);
    pthread_join(&id, NULL);
}
