// CRAM
// The worker may advance ghost_a before main locks m, but not while main holds
// m. Once main copies ghost_a to ghost_b while holding m, they remain equal
// until m is released.
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

    pthread_create(&thread, NULL, (void*)fun, NULL);

    pthread_mutex_lock(&m);

    x++;
    x++;

    pthread_mutex_unlock(&m);
    pthread_join(thread, NULL);

    return 0;
}
