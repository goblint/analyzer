// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins

#include <pthread.h>
#include <assert.h>

int x = 0;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void* T1_Singleton_With_Uninit(void* arg){
	pthread_mutex_lock(&lock);
	x = 3;
	pthread_mutex_unlock(&lock);
	return NULL;
}

void* T2_Singleton_With_Uninit(void* arg){
	pthread_mutex_lock(&lock);
	x = 5;
	pthread_mutex_unlock(&lock);
	return NULL;
}

int main() {
	pthread_t t1;
    pthread_t t2;
    pthread_create(&t1, 0, T1_Singleton_With_Uninit, 0);
    pthread_create(&t2, 0, T2_Singleton_With_Uninit, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);

	assert(x <= 5);
	return 0;
}
