//PARAM: --set ana.modular.funs "['foo']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'"

#include <stdio.h>
#include <pthread.h>
#include <goblint.h>

int *escaped_local;

void foo(void *arg) {
    int local = 5;
    escaped_local = &local;
	__goblint_check(local == 5); //UNKNOWN!
}

void *thread_f(void *arg){
	foo(NULL);
	return NULL;
}

int main() {
    pthread_t thread;
    pthread_create(&thread, NULL, thread_f, NULL);
    *escaped_local = 10;
    pthread_join(thread, NULL);
    return 0;
}