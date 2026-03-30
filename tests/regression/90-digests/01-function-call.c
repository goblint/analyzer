//PARAM: --set "solver" "bu"
// Test that function calls are handled properly when the digest changes during the function call

#include <pthread.h>
#include <goblint.h>

pthread_mutex_t m1;

void my_lock(pthread_mutex_t* m) {
	pthread_mutex_lock(m);
}


int main(int argc, char const *argv[])
{
	int top;
	pthread_mutex_t* ptr;
	ptr = &m1;


	my_lock(ptr);

	if(top) {
		// possible double-locking error
		pthread_mutex_lock(ptr); //WARN
	}

	pthread_mutex_unlock(ptr); //NOWARN

	return 0;
}