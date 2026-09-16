// CRAM
// The worker has three phases because ghost_a is advanced at two statements.
#include<pthread.h>
#include<goblint.h>

int x;

void fun() {
    x++;
    x++;
}

int main(void) {
    pthread_t thread;
    pthread_create(&thread, NULL, (void*)fun, NULL);

    x++;

    pthread_join(thread, NULL);

    return 0;
}
