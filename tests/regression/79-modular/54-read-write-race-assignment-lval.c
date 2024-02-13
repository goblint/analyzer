//PARAM: --enable modular --set ana.modular.funs "['write']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'"
#include<stdlib.h>
#include<pthread.h>
#include<stdio.h>

int g = 0;
int *p = &g;

void write(int** i){
	// race when reading *i
	**i = 23;
}

void change_pointer(int* i){
	p = i; // RACE
}

void *thread1(void *pp){
	int **pp_typed = (int**)pp;
	write(pp_typed); // RACE
	return NULL;
}

void *thread2(void *p){
	int j = 0;
	change_pointer(&j);
	return NULL;
}

int main(){
	pthread_t t1;
	pthread_t t2;
	pthread_create(&t1, NULL, thread1, &p);
	pthread_create(&t2, NULL, thread2, NULL);
}

