//PARAM: --set ana.modular.funs "['change_pointer', 'write_through_pointer']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval
#include <stdlib.h>
#include <pthread.h>
#include <goblint.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

int global = 1;
int **pp = NULL;

void *change_pointer(void *p){
	*pp = &global;
	return NULL;
}

void *write_through_pointer(void *p){
	if(pp != NULL && *pp != NULL){
		**pp = 12;
	}
	return NULL;
}

void *change_pointer_wrapper(void *p){
	pthread_mutex_lock(&mutex);
	void *r = change_pointer(p);
	pthread_mutex_unlock(&mutex);
	return r;
}

void *write_through_pointer_wrapper(void *p){
	pthread_mutex_lock(&mutex);
	void *r = write_through_pointer(p);
	pthread_mutex_unlock(&mutex);
	return r;
}

int main(){
	pthread_t t1, t2;
	pp = malloc(sizeof(int*));
	*pp = NULL;

	pthread_create(&t1, NULL, change_pointer_wrapper, NULL);
	pthread_create(&t2, NULL, write_through_pointer_wrapper, NULL);

	if(pp != NULL && *pp != NULL){
		printf("%d\n", **pp);
		__goblint_check(**pp == 12); //UNKNOWN!
	}
	__goblint_check(global == 12); //UNKNOWN!
	return 0;
}