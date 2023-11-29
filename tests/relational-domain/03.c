#include <stdio.h>
#include <pthread.h>

#define NUM_THREADS 10
#define NUM_ITERATIONS 10000

int x=0;
int y=0;

void *thread(void *arg){
    for(int i=0; i<NUM_ITERATIONS; i++){
        x++;
    }
    return NULL;
}

int main(){
    pthread_t threads[NUM_THREADS];
    for(int i=0; i< NUM_THREADS; i++){
        pthread_create(&threads[i], NULL, thread, NULL);
    }

    for(int i=0; i< NUM_ITERATIONS; i++){
        y++;
    }

    for(int i=0; i < NUM_THREADS; i++){
        pthread_join(threads[i], NULL);
    }

    __goblint_check(x!=y);
    return 0;
}

/* This test case increments 2 shared variables (x and y) concurrently in a loop. After the thread finish their execution, the main thread checks if the final values of x and y are equal. */