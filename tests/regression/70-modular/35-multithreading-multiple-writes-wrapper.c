//PARAM: --set ana.modular.funs "['change_pointer', 'write_through_pointer']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval
#include <stdlib.h>
#include <pthread.h>
#include <goblint.h>

int g = 0;
int **pp = NULL;

void *change_pointer(void *p){
	// 1. Assign pointer to g to pp
	*pp = &g;

	// 2. Assign pointer to allocated memory to pp
	int *ptr = malloc(sizeof(int));
	*ptr = 0;
	*pp = ptr;

	return NULL;
}

void *write_through_pointer(void *p){
	**pp = 12;
	int i = **pp;
	return NULL;
}

void *change_pointer_wrapper(void *p){
	void *r = change_pointer(p);
	return r;
}

void *write_through_pointer_wrapper(void *p){
	void *r = write_through_pointer(p);
	int i = **pp;
	__goblint_check(i == 12); //UNKNOWN!

	int j = g;
	__goblint_check(j == 12); //UNKNOWN!
	return r;
}

int main(){
	pthread_t t1, t2;
	pp = malloc(sizeof(int*));
	*pp = NULL;

	pthread_create(&t1, NULL, change_pointer_wrapper, NULL);
	pthread_create(&t2, NULL, write_through_pointer_wrapper, NULL);

	if(pp != NULL && *pp != NULL){
		int i = **pp;
		__goblint_check(i == 12); //UNKNOWN!
	}
	int j = g;
	__goblint_check(g == 12); //UNKNOWN!
	__goblint_check(j == 12); //UNKNOWN!

	return 0;
}