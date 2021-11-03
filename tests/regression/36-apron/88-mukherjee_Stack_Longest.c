// SKIP

#include <pthread.h>
#include "assert.h"

int SIZE = 800;
int OVERFLOW  = -1;
int top = 0;
int flag = 0;
int arr1;
int arr2;

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void* T1_Stack_Longest(void* arg) {
    int i;
    for(i=0; i<SIZE; i++) {
        pthread_mutex_lock(&m);
        assert(top != SIZE);
        
        if (top != SIZE) {
            top++;
        }
        flag = 1;
        pthread_mutex_unlock(&m);
    }
    return NULL;
}

void* T2_Stack_Longest(void* arg) {
    int i;
    for(i=0; i<SIZE; i++) {
        pthread_mutex_lock(&m);
        if (flag == 1) {
            assert(top != 0);
            if (top != 0)
                top--;
        }
        pthread_mutex_unlock(&m);
    }
    return NULL;
}

int main() {
	pthread_t t1;
    pthread_t t2;
    pthread_create(&t1, 0, T1_Stack_Longest, 0);
    pthread_create(&t2, 0, T2_Stack_Longest, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);
    return 0;
}