// SKIP
// PARAM: --enable ana.int.interval_set --enable ana.int.interval_threshold_widening

#include <pthread.h>
#include <goblint.h>

int g;

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void* fun(void* arg) {
    pthread_mutex_lock(&m);
    if(g < 100) {
        g++;
    };
    pthread_mutex_unlock(&m);
}


int main() {
    pthread_t t1;
    pthread_create(&t1, 0, fun, 0);

    pthread_mutex_lock(&m);
    if(g < 100) {
        g++;
    };
    pthread_mutex_unlock(&m);

    pthread_join(t1, 0);

    __goblint_check(g <= 100);

	return 0;
}
