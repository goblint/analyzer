// SKIP PARAM: --set ana.modular.funs "['write', 'read']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'"
#include<stdlib.h>
#include<pthread.h>
#include<stdio.h>

int g = 0;
int h = 0;

void read(int* i){
	int x = *i;
	printf("%d\n", x);
}

void write(int* i){
	*i = 23;
}

void *thread1(void *p){
	read(&g); // RACE
	read(&h); // NORACE
	return NULL;
}

void *thread2(void *p){
	write(&g); // RACE
	read(&h); // NORACE
	return NULL;
}

int main(){
	pthread_t t1;
	pthread_t t2;
	pthread_create(&t1, NULL, thread1, NULL);
	pthread_create(&t2, NULL, thread2, NULL);
}

