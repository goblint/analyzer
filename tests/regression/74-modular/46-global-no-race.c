// PARAM: --set ana.modular.funs "['write']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'"  --set ana.activated[+] "'used_globals'"
#include<stdlib.h>
#include<pthread.h>

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

int g = 0;

int write(int *p){
	g = 23;
}

void *thread1(void *p){
	pthread_mutex_lock(&m);
	write(p); // NORACE
	pthread_mutex_unlock(&m);
	return NULL;
}

void *thread2(void *p){
	pthread_mutex_lock(&m);
	g = 12; // NORACE
	pthread_mutex_unlock(&m);
	return NULL;
}

int main(){
	pthread_t t1;
	pthread_t t2;
	pthread_create(&t1, NULL, thread1, NULL);
	pthread_create(&t2, NULL, thread2, NULL);
}

