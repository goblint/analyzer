// CRAM
// The worker can only advance ghost_a while holding m. Since main creates the
// worker while holding m and keeps holding it at the invariant, ghost_a cannot
// advance there.
#include<pthread.h>
#include<goblint.h>

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;
int x;

void fun() {
    pthread_mutex_lock(&m);
    x++;
    pthread_mutex_unlock(&m);
}

int main(void) {
    pthread_t thread;

    pthread_mutex_lock(&m);
    pthread_create(&thread, NULL, (void*)fun, NULL);

    x++;

    pthread_mutex_unlock(&m);
    pthread_join(thread, NULL);

    return 0;
}
