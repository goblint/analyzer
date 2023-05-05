//SKIP PARAM: --set ana.modular.funs "['write_global']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval
#include <stdlib.h>
#include <pthread.h>
#include <goblint.h>

int g = 0;

void *write_global(void *p){
	g = 12;
	return NULL;
}

int main(){
	pthread_t t;
	pthread_create(&t, NULL, write_global, NULL);

	int j = g;
	__goblint_check(g == 12); //UNKNOWN!
	__goblint_check(j == 12); //UNKNOWN!

	__goblint_check(g <= 12);
	__goblint_check(j <= 12);

	return 0;
}