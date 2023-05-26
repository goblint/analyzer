#include <stdio.h>
#include <pthread.h>

#define NUM_THREADS 5

void *threadFunction(void *arg) {
    int threadID = *(int *)arg;
    printf("Thread %d is running.\n", threadID);
    // Perform some task in the thread
    printf("Thread %d completed.\n", threadID);
    pthread_exit(NULL);
}

int main() {
    pthread_t threads[NUM_THREADS];
    int threadArgs[NUM_THREADS];
    int i, result;

    // Create threads
    for (i = 0; i < NUM_THREADS; i++) {
        threadArgs[i] = i;
        result = pthread_create(&threads[i], NULL, threadFunction, &threadArgs[i]);
        if (result != 0) {
            printf("Error creating thread %d. Exiting program.\n", i);
            return -1;
        }
    }

    // Wait for threads to finish
    for (i = 0; i < NUM_THREADS; i++) {
        result = pthread_join(threads[i], NULL);
        if (result != 0) {
            printf("Error joining thread %d. Exiting program.\n", i);
            return -1;
        }
    }

    printf("All threads have completed. Exiting program.\n");

    return 0;
}
