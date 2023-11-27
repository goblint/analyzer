#include <pthread.h>

typedef struct foo_struct {
	union {
		int i;
		struct inner_struct {
			int head, tail;
		} coin;
	} un;
} foo_t;

foo_t str;

void *read(void *ptr){
	int x = str.un.coin.head; // RACE!
	int a = 32;
	return NULL;
}

void *write(void *ptr){
	str.un.coin.head = 23; // RACE!
	return NULL;
}

int main(){
	pthread_t r;
	pthread_t w;

	pthread_create(&w, NULL, write, NULL);
	pthread_create(&r, NULL, read, NULL);

	return 0;
}
