// PARAM: --set ana.activated[+] thread 
#include <stdlib.h>
#include <pthread.h>

void *thread(void * arg)
{
	return 0;
}


int main()
{
	int tid;
	pthread_t *t;

	t = (pthread_t *)malloc(sizeof(pthread_t) * 16);

	tid=0;
	pthread_create(&t[tid], 0, thread, 0);
	tid++;
	pthread_create(&t[tid], 0, thread, 0);
	
	tid=0;
	pthread_join(t[tid], 0);
	tid++;
	pthread_join(t[tid], 0);

	return 0;
}

