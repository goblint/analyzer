//PARAM: --enable modular --set ana.modular.funs "['foo']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'"

#include <stdio.h>
#include <pthread.h>
#include <goblint.h>

int global;
int *global_pointer;
int *escaped_local;

void foo() {
    int local = 5;
    escaped_local = &local;

    __goblint_check(global_pointer == &local); //UNKNOWN!
    __goblint_check(global_pointer != &global); //UNKNOWN!
    __goblint_check(global_pointer != escaped_local); //UNKNOWN!
}

void *thread_f(void *arg){
	foo();
	return NULL;
}

int main() {
    pthread_t thread;
    pthread_create(&thread, NULL, thread_f, NULL);
    global_pointer = escaped_local;
    pthread_join(thread, NULL);
    return 0;
}