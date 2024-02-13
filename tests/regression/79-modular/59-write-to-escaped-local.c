//PARAM: --enable modular --set ana.modular.funs "['foo']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'"--set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'"

#include <stdio.h>
#include <pthread.h>
#include <goblint.h>

int global;
int global2;
int **escaped_local;

void foo() {
    int *local = NULL;
    escaped_local = &local;

    __goblint_check(local == NULL); //UNKNOWN!
    if(local != NULL){
        __goblint_check(*local == &global); //UNKNOWN!
    }
}

void *thread_f(void *arg){
	foo();
	return NULL;
}

int main() {
    int top;
    pthread_t thread;
    pthread_create(&thread, NULL, thread_f, NULL);
    if(top){
        *escaped_local = &global;
    } else {
        *escaped_local = &global2;
    }
    pthread_join(thread, NULL);
    return 0;
}