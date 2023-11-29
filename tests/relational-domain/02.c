#include <stdio.h>
#include <pthread.h>

int x=0;
int y=0;

void *thread1(void *arg){
    if(x==0){
        y=1;
    }
    return NULL;
}

void *thread2(void *arg){
    x=1;
    return NULL;
}

int main(){
    pthread_t thread1_id, thread2_id;

    pthread_create(&thread1_id, NULL, thread1, NULL);
    pthread_create(&thread2_id, NULL, thread2, NULL);

    pthread_join(thread1_id, NULL);
    pthread_join(thread2_id, NULL);

    __goblint_check(x!=y);

    return 0;
}

/* this test case evaluates whether analyzer can correctly analyze the potential equality between x and y considering the different paths taken by the threads. */
