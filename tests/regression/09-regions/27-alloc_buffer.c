//SKIP! PARAM: --set ana.activated[+] "'region'" --set ana.activated[+] "'escape'"
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

void *t_fun() {
    char* buf = (char*) malloc(1);
    free(buf); // NORACE
    return NULL;
}


int main () {
    pthread_t t1;
    pthread_t t2;

    pthread_create(&t1,NULL, t_fun,NULL);
    pthread_create(&t2,NULL, t_fun,NULL);

    pthread_exit(0);

}
