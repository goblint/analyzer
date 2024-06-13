// PARAM: --enable ana.int.interval --enable ana.int.interval_threshold_widening --set ana.int.interval_threshold_widening_constants comparisons --enable solvers.td3.narrow-sides.enabled --enable solvers.td3.narrow-sides.stable
#include <pthread.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int glob = 0;

void *thread(void *data) {
    int ub = *(int*)data;
    for(int i = 0; i < ub; i++) {
        pthread_mutex_lock(&mutex);
        glob = i + 2;
        pthread_mutex_unlock(&mutex);
    }
    return NULL;
}

int main(void)
{
    int id;
    int ub = 256;
    pthread_create(&id, NULL, thread, &ub);
    pthread_mutex_lock(&mutex);
    __goblint_check(glob >= 0);
    __goblint_check(glob <= 258);
    pthread_mutex_unlock(&mutex);
    pthread_join(&id, NULL);
}
