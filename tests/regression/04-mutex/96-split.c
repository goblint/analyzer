#include<pthread.h>

pthread_mutex_t m1;
pthread_mutex_t m2;

int main(int argc, char const *argv[])
{
    int top;
    pthread_mutex_t* ptr;
    ptr = &m1;

    if(top) {
        ptr = &m2;
    }

    pthread_mutex_lock(ptr);
    pthread_mutex_unlock(ptr); //NOWARN

    return 0;
}