//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>
#include <pthread.h>

int x=0;
int y=0;

void *thread1(void *arg){
    x=1;
    y=1;
    return NULL;
}

void *thread2(void *arg){
    y=2;
    x=2;
    return NULL;
}

int main(){
    pthread_t thread1_id, thread2_id;

    pthread_create(&thread1_id, NULL, thread1, NULL);
    pthread_create(&thread2_id, NULL, thread2, NULL);

    pthread_join(thread1_id, NULL);
    pthread_join(thread2_id, NULL);

    __goblint_check(x==y); //UNKNOWN！
    __goblint_check(x!=y); //UNKNOWN！

    return 0;
}

//this test case verifies whether the analyzer can correctly represent and maintain the relationship between x and y even when they are modfied concurrectly.

