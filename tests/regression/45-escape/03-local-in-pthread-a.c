#include <pthread.h>
#include <assert.h>

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>


void *foo(void* p){
    sleep(2);
    int* ip = *((int**) p);
    printf("ip is %d\n", *ip);
    // To check that in (01) even without modification both &x and &x2 are possible here
    assert(*ip == 0); //UNKNOWN!
    assert(*ip == 35); //UNKNOWN!
    return NULL;
}

int main(){
    int x = 0;
    int *xp = &x;
    int** ptr = &xp;
    int x2 = 35;
    pthread_t thread;
    pthread_create(&thread, NULL, foo, ptr);
    *ptr = &x2;
    sleep(4); // to make sure that we actually fail the assert when running.
    pthread_join(thread, NULL);
    return 0;
}
