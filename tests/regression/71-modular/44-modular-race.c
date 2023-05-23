// PARAM: --set ana.modular.funs "['write']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"
#include<stdlib.h>
#include<pthread.h>


int write(int *p){
	*p = 23; // RACE
}

void *thread1(void *p){
	write(p);
	return NULL;
}

void *thread2(void *p){
	int *ip = (int *) p;
	*ip = 12; // RACE
}

int main(){
	pthread_t thread;
	int j = 0;

	pthread_create(&thread, NULL, thread1, &j);
	pthread_create(&thread, NULL, thread2, &j);
}

