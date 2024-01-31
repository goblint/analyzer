#include<pthread.h>

pthread_mutex_t m1;
pthread_mutex_t m2;
pthread_mutex_t* ptr;

void other() {
    int top;
    ptr = &m2;

    if(top) {
        ptr = &m1;
    }
}

int main(int argc, char const *argv[])
{
    int top;

    ptr = &m1;

    if(top) {
        ptr = &m2;
    }

    pthread_t mischievous;
    pthread_create(&mischievous, NULL, other, NULL);


    pthread_mutex_lock(ptr);

    // This has to produce a warning, as the other thread may have changed what
    // ptr points to such that it's not the same mutex being unlocked here.
    pthread_mutex_unlock(ptr); //WARN

    return 0;
}