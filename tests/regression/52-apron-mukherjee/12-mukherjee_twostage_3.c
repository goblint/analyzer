// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --enable ana.apron.threshold_widening

#include <pthread.h>
#include <assert.h>

int data1value = 0;
int data2value = 0;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

void* iTThread_1(void* arg){
    pthread_mutex_lock(&lock);
    data1value = 1;
    pthread_mutex_unlock(&lock);

    pthread_mutex_lock(&lock);
    data2value = data1value + 1;
    pthread_mutex_unlock(&lock);
    return 0;
}

void* iTThread_2(void* arg){
    pthread_mutex_lock(&lock);
    data1value = 1;
    pthread_mutex_unlock(&lock);

    pthread_mutex_lock(&lock);
    data2value = data1value + 1;
    pthread_mutex_unlock(&lock);
	return 0;
}

void* iRThread_1(void* arg){
	int t1 = -1;
	int t2 = -1;

	pthread_mutex_lock(&lock);
	t1 = data1value;
	t2 = data2value;
	pthread_mutex_unlock(&lock);

	if(t1 != 0) {
		assert(t2 != (t1+1)); //UNKNOWN!
		assert(t2 == (t1+1)); //UNKNOWN!
	}
	return 0;
}

int main() {
	data1value = 0;
	data2value = 0;

	pthread_t t1;
    pthread_t t2;
    pthread_t t3;
    pthread_create(&t1, 0, iTThread_1, 0);
    pthread_create(&t2, 0, iTThread_2, 0);
    pthread_create(&t3, 0, iRThread_1, 0);
    pthread_join(t1, 0);
    pthread_join(t2, 0);
    pthread_join(t3, 0);

	return 0;
}
