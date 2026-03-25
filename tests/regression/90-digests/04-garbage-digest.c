//PARAM: --set solver bu --enable ana.int.interval
#include <pthread.h>
#include <stdio.h>
#include <goblint.h>
pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;

int main(){
	int z = 0;

	pthread_mutex_lock(&m1);
	int i;

	for(i = 0; i < 10; i++){
		printf("Iteration %d\n", i);
	}
	if (i == 11){
		// This is not reachable, but introduces a garbage value for z that should be garbage collected, so that the check below is not unknown.
		z = 7;
	}

	__goblint_check(z == 0); //TODO
	pthread_mutex_unlock(&m1);
	return 0;
}