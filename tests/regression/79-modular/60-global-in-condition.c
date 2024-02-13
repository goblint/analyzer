//PARAM: --enable modular --set ana.modular.funs "['foo']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'"

#include <pthread.h>
#include <stdlib.h>
#include <goblint.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

int g = 4;

int foo(){
	pthread_mutex_lock(&mutex);
	if(g == 4){
		pthread_mutex_unlock(&mutex);

		pthread_mutex_lock(&mutex);
		__goblint_check(g == 4); // UNKNOWN!
		pthread_mutex_unlock(&mutex);
	}
}

void *thread_f(void *arg){
	foo();
	return NULL;
}

int main(){;
	pthread_t thread;
	pthread_create(&thread, NULL, thread_f, NULL);

	foo();

	pthread_mutex_lock(&mutex);
	g = 5;
	pthread_mutex_unlock(&mutex);
}

