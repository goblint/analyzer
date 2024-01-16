// PARAM: --enable modular --set ana.modular.funs "['write']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'"  --set ana.activated[+] "'used_globals'"
#include<stdlib.h>
#include<pthread.h>


int g = 0;
int h = 0;
int i = 0;

void write(int* i){
	*i = 23;
}

void *thread1(void *p){
	write(&g); // RACE
	write(&i); // NORACE
	return NULL;
}

void *thread2(void *p){
	write(&g); // RACE
	write(&h); // NORACE
	return NULL;
}

int main(){
	pthread_t t1;
	pthread_t t2;
	pthread_create(&t1, NULL, thread1, NULL);
	pthread_create(&t2, NULL, thread2, NULL);
}

