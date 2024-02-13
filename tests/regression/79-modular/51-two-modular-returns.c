// PARAM: --enable modular --set ana.modular.funs "['value']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'"
#include<stdlib.h>
#include<pthread.h>


int g = 0;
int h = 0;
int i = 0;

int value(){
	return 23;
}

void *thread1(void *p){
	// Race on g
	g = value();  // RACE
	i = value(); // NORACE
	return NULL;
}

void *thread2(void *p){
	// Race on g
	g = value();  // RACE
	h = value(); // NORACE
	return NULL;
}

int main(){
	pthread_t t1;
	pthread_t t2;
	pthread_create(&t1, NULL, thread1, NULL);
	pthread_create(&t2, NULL, thread2, NULL);
}

