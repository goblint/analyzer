//PARAM: --set "solver" "bu"
// Test that function calls are handled properly when the digest changes during the function call

#include <pthread.h>
#include <goblint.h>

pthread_mutex_t m1;
pthread_mutex_t m2;

pthread_mutex_t* ptr;

void my_lock() {
	pthread_mutex_lock(ptr);
}


int main(int argc, char const *argv[])
{
	int top;

	ptr = &m1;

	if(top){
		ptr = &m2;
	}

	my_lock();

	pthread_mutex_unlock(ptr); //NOWARN

	return 0;
}