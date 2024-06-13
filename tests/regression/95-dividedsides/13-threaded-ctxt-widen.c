// PARAM: --enable ana.int.interval_set --set ana.context.gas_value 0 --enable solvers.td3.narrow-sides.enabled --enable solvers.td3.narrow-sides.stable
#include <pthread.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int glob = 0;
int glob2 = 0;

void *thread(void *) {    
    f(10);
    return NULL;
}

void f(int i) {
    pthread_mutex_lock(&mutex);
    glob = i;
    pthread_mutex_unlock(&mutex);
    if (i > 0) {
        f(i - 1);
    }
}

void *thread2(void *) {
    g(10);
    return NULL;
}

void g(int i) {
    pthread_mutex_lock(&mutex);
    glob2 = i;
    pthread_mutex_unlock(&mutex);
    if (i != 0) {
        g(i - 1);
    }
}

int main(void)
{
    int id;
    pthread_create(&id, NULL, thread, NULL);
    pthread_create(&id, NULL, thread2, NULL);
    pthread_mutex_lock(&mutex);
    __goblint_check(glob >= 0 && glob <= 10);
    __goblint_check(glob2 >= 0 && glob2 <= 10); //UNKNOWN (operator != does not permit narrowing)
    pthread_mutex_unlock(&mutex);
}
