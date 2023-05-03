//PARAM: --set ana.modular.funs "['foo', 'bar']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval
#include <stdlib.h>
#include <pthread.h>
#include <goblint.h>

int g = 0;
int **pp = NULL;

void *foo(void *p){
	*pp = &g;
	return NULL;
}

void *bar(void *p){
	**pp = 12;
	return NULL;
}

int main(){
	pthread_t t1, t2;
	pp = malloc(sizeof(int*));
	*pp = NULL;

	pthread_create(&t1, NULL, foo, NULL);
	// pthread_create(&t2, NULL, bar, NULL);
	bar(NULL);

	if(pp != NULL && *pp != NULL){
		__goblint_check(**pp == 12); //UNKNOWN
	}
	__goblint_check(g == 12); //UNKNOWN
	return 0;
}