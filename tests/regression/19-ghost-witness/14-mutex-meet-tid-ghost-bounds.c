// CRAM
// Ghost phases recover protected values that plain mutex-meet-tid cannot validate.
#include<pthread.h>
#include<goblint.h>

int x;
pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;


void fun() {
    pthread_mutex_lock(&m);
    x = 1;
    pthread_mutex_unlock(&m);

    pthread_mutex_lock(&m);
    x = 2;
    pthread_mutex_unlock(&m);
}

int main(void) {
    pthread_t thread;
    pthread_create(&thread, NULL, (void*)fun, NULL);

    pthread_mutex_lock(&m);
    
    pthread_mutex_unlock(&m);
    pthread_join(thread, NULL);

    return 0;
}
