//PARAM: --disable warn.assert
#include<pthread.h>
#include<assert.h>
int counter = 0;
pthread_mutex_t lock1;

void* producer(void* param) {
    pthread_mutex_lock(&lock1);
    counter = 0;
    pthread_mutex_unlock(&lock1);
}

void* consumer(void* param) {
    pthread_mutex_lock(&lock1);
    int bla = counter >= 0;
    // This should not produce a warning about the privatization being unsound
    assert(counter >= 0); //NOWARN
    pthread_mutex_unlock(&lock1);
}


int main() {
    pthread_t thread;
    pthread_t thread2;
    pthread_create(&thread,NULL,producer,NULL);
    pthread_create(&thread2,NULL,consumer,NULL);
}
