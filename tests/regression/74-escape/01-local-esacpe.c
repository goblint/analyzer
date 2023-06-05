#include<stdlib.h>
#include<pthread.h>
#include<goblint.h>
#include<unistd.h>

int g = 0;
int *p = &g;

int let_escape(){
	int x = 23;
	g = 23;

	__goblint_check(x == 23);
	p = &x;
	sleep(5);
	__goblint_check(x == 23); //UNKNOWN!
}

void *thread1(void *pp){
	let_escape();
	return NULL;
}

void write_through_pointer(){
	sleep(2);
	*p = 1;
}

void *thread2(void *p){
	write_through_pointer();
	return NULL;
}

int main(){
	pthread_t t1;
	pthread_t t2;
	pthread_create(&t1, NULL, thread1, NULL);
	pthread_create(&t2, NULL, thread2, NULL);
	pthread_join(t1, NULL);
	pthread_join(t2, NULL);
}

