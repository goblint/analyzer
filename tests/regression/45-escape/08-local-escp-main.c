//PARAM: --enable ana.int.interval
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

	int y = x;
	return NULL;
}

int main(){
	pthread_t t1;
	pthread_t t2;
	pthread_create(&t1, NULL, thread1, NULL);
	sleep(1);
	*p = 1;
}

