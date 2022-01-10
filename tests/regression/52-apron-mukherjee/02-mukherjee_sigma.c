// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --sets ana.apron.privatization mutex-meet-tid

#include <stdlib.h>
#include <pthread.h>
#include <string.h>
#include <assert.h>

const int SIGMA = 4;

int array_0, array_1, array_2, array_3;
int array_index;

pthread_mutex_t mut_lock = PTHREAD_MUTEX_INITIALIZER;

void *thread1(void * arg)
{
    assert(array_index <= 4);
    pthread_mutex_lock(&mut_lock);
	switch (array_index) {
        case 0:
            array_0 = 1;
            break;
        case 1:
            array_1 = 1;
            break;
        case 2:
            array_2 = 1;
            break;
        case 3:
            array_3 = 1;
            break;
    }
    pthread_mutex_unlock(&mut_lock);
	return 0;
}

void *thread2(void * arg)
{
    assert(array_index <= 4);
    pthread_mutex_lock(&mut_lock);
	switch (array_index) {
        case 0:
            array_0 = 1;
            break;
        case 1:
            array_1 = 1;
            break;
        case 2:
            array_2 = 1;
            break;
        case 3:
            array_3 = 1;
            break;
    }
    pthread_mutex_unlock(&mut_lock);
	return 0;
}

void *thread3(void * arg)
{
    assert(array_index <= 4);
    pthread_mutex_lock(&mut_lock);
	switch (array_index) {
        case 0:
            array_0 = 1;
            break;
        case 1:
            array_1 = 1;
            break;
        case 2:
            array_2 = 1;
            break;
        case 3:
            array_3 = 1;
            break;
    }
    pthread_mutex_unlock(&mut_lock);
	return 0;
}

void *thread4(void * arg)
{
    assert(array_index <= 4);
    pthread_mutex_lock(&mut_lock);
	switch (array_index) {
        case 0:
            array_0 = 1;
            break;
        case 1:
            array_1 = 1;
            break;
        case 2:
            array_2 = 1;
            break;
        case 3:
            array_3 = 1;
            break;
    }
    pthread_mutex_unlock(&mut_lock);
	return 0;
}


int main()
{
	int sum;

	pthread_t t1;
    pthread_t t2;
    pthread_t t3;
    pthread_t t4;

    pthread_create(&t1, 0, thread1, 0);
    pthread_mutex_lock(&mut_lock);
    array_index++;
    pthread_mutex_unlock(&mut_lock);

    pthread_create(&t2, 0, thread2, 0);
    pthread_mutex_lock(&mut_lock);
    array_index++;
    pthread_mutex_unlock(&mut_lock);

    pthread_create(&t3, 0, thread3, 0);
    pthread_mutex_lock(&mut_lock);
    array_index++;
    pthread_mutex_unlock(&mut_lock);

    pthread_create(&t4, 0, thread4, 0);
    pthread_mutex_lock(&mut_lock);
    array_index++;
    pthread_mutex_unlock(&mut_lock);

    pthread_join(t1, 0);
    pthread_join(t2, 0);
    pthread_join(t3, 0);
    pthread_join(t4, 0);

	sum = array_0 + array_1 + array_2 + array_3;

	assert(sum == SIGMA);  //UNKNOWN! <-- wrong, different threads might use the same array offset when writing

	return 0;
}
