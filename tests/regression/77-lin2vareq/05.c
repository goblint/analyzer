#include <stdio.h>
#include <pthread.h>

int x =0;
char y='a';

void *thread1(void *arg){
    if(x==0){
        y='b';
    }
    return NULL;
}

void *thread2(void *arg){
    x =1;
    return NULL;
}

int main(){
    pthread_t thread1_id, thread2_id;

    pthread_create(&thread1_id, NULL, thread1, NULL);
    pthread_create(&thread2_id, NULL, thread2, NULL);

    pthread_join(thread1_id, NULL);
    pthread_join(thread2_id, NULL);

    __goblint_check(x!=y); //UNKNOWN!

    return 0;
}

/* This test case uses 2 different types of variable, an integer and a character. It ensures that analyzer can track 2 variable equalities even when the variables have different types and sizes. */