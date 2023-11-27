// PARAM: --set ana.modular.funs "['value']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'"  --set ana.activated[+] "'used_globals'"
#include<stdlib.h>
#include<pthread.h>


int g = 0;

int value(){
	return 23;
}

void *thread1(void *p){
	// Race on g
	g = value();  // RACE
	return NULL;
}

void *thread2(void *p){
	g = 12; // RACE
	return NULL;
}

int main(){
	pthread_t t1;
	pthread_t t2;
	pthread_create(&t1, NULL, thread1, NULL);
	pthread_create(&t2, NULL, thread2, NULL);
}

