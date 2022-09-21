// SKIP PARAM: --set solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','apron','escape']" --set ana.path_sens[+] threadflag --set ana.base.privatization none --set ana.relation.privatization mutex-meet-tid
#include <pthread.h>
#include <assert.h>
#include <stdio.h>
#include <unistd.h>

void *foo(void* p){
    sleep(2);
    int top;
    return top;
}

int main(){
    pthread_t thread;
    int y = 8;
    pthread_create(&thread, NULL, foo, NULL);
    pthread_join(thread, &y);
    assert(y==8); //UNKNOWN!
    return 0;
}
