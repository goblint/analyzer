// PARAM: --set ana.malloc.unique_address_count 3
#include<pthread.h>
static int iRThreads = 1;
static int data1Value = 0;
pthread_mutex_t *data1Lock;

void *funcA(void *param) {
    pthread_mutex_lock(data1Lock);
    data1Value = 1; //NORACE
    pthread_mutex_unlock(data1Lock);
}

int main(int argc, char *argv[]) {
    data1Lock = (pthread_mutex_t *) malloc(sizeof(pthread_mutex_t));

    pthread_t one;
    pthread_t other[3];

    pthread_create(&one, ((void *)0), &funcA, ((void *)0));

    for (int i = 0; i < 3; i++) {
      pthread_create(&other[i], ((void *)0), &funcA, ((void *)0));
    }

    return 0;
}
