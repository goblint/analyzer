#include <pthread.h>

static pthread_t sid1 ;
static pthread_t sid2 ;
static pthread_t sid3 ;

void *fn1(void * p){
	return NULL;
}

void *fn2(void) {
    return NULL;
}

extern void *fn3(void * a);


int main() { 
	/* normal call to fn1 */
	pthread_create(&sid1, NULL, fn1, NULL);
	
	/* ignore parameter (cast parameter to void) call */ 
	pthread_create(&sid2, NULL, (void *(*)(void * )) (& fn2), NULL);
	
	/* we create a unknown thread -- that can't be good */
	pthread_create(&sid3, NULL, &fn3, NULL);

	pthread_join(sid3, NULL);
	
	pthread_join(sid2, NULL);
	
	pthread_join(sid1, NULL);
	
	return 0;
}


