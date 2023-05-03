//PARAM: --set ana.modular.funs "['change_pointer', 'write_through_pointer']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval
#include <stdlib.h>
#include <pthread.h>
#include <goblint.h>

int global = 0;
int **pp = NULL;

void *change_pointer(void *p){
	*pp = &global;
	return NULL;
}

void *write_through_pointer(void *p){
	if(p != NULL && *pp != NULL){
		**pp = 12;
	}
	return NULL;
}

int main(){
	pthread_t t1, t2;
	pp = malloc(sizeof(int*));
	*pp = NULL;

	pthread_create(&t1, NULL, change_pointer, NULL);
	// pthread_create(&t2, NULL, write_through_pointer, NULL);
	write_through_pointer(NULL);

	if(pp != NULL && *pp != NULL){
		__goblint_check(**pp == 12); //UNKNOWN!
	}
	__goblint_check(global == 12); //UNKNOWN!
	return 0;
}