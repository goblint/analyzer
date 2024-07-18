// PARAM:  --enable ana.int.interval
#include<stdlib.h>
#include<pthread.h>
#include<goblint.h>
#include<unistd.h>

int g = 0;
int *p = &g;


void *thread1(void *pp){
	int x = 23;
	__goblint_check(x == 23);
	p = &x;
	sleep(2);
	__goblint_check(x == 23); //UNKNOWN!
	__goblint_check(x <= 23);
	__goblint_check(x >= 1);

	return NULL;
}

void *thread2(void *ignored){
	sleep(1);
	int *i = p;
	*p = 1;
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

