// LOCAL_TERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

// Thread function
void* printPID(void* arg) {
    pid_t pid = getpid();
    pthread_t tid = pthread_self();
    printf("Thread ID: %lu, Process ID: %d\n", (unsigned long)tid, pid);
    return NULL;
}

int main() {
    // Create three threads
    pthread_t thread1, thread2, thread3;
    pthread_create(&thread1, NULL, printPID, NULL);
    pthread_create(&thread2, NULL, printPID, NULL);
    pthread_create(&thread3, NULL, printPID, NULL);

    // Wait for all threads to finish
    pthread_join(thread1, NULL);
    pthread_join(thread2, NULL);
    pthread_join(thread3, NULL);

    return 0;
}
