// SKIP PARAM: --set solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','assert','apron','escape']" --set ana.base.privatization none --set ana.apron.privatization top
// Copy of 45 01 for apron
#include <pthread.h>
#include <assert.h>
#include <stdio.h>
#include <unistd.h>


void *foo(void* p){
    sleep(2);
    int* ip = *((int**) p);
    printf("ip is %d\n", *ip);
    *ip = 42;
    return NULL;
}

int main(){
    int x = 0;
    int *xp = &x;
    int** ptr = &xp;
    int x2 = 35;
    pthread_t thread;
    pthread_create(&thread, NULL, foo, ptr);
    assert(x2 == 35);
    *ptr = &x2;
    sleep(4); // to make sure that we actually fail the assert when running.
    assert(x2 == 42); //UNKNOWN!
    assert(x2 == 35); //UNKNOWN!
    pthread_join(thread, NULL);
    return 0;
}
