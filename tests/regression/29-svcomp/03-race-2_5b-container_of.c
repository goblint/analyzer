// Copied from sv-benchmarks
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define offsetof(TYPE, MEMBER) ((unsigned long) &((TYPE *)0)->MEMBER)

#define container_of(ptr, type, member) ({                      \
	const typeof( ((type *)0)->member ) *__mptr = (ptr);    \
	(type *)( (char *)__mptr - offsetof(type,member) );})

extern void abort(void);
#include <goblint.h>
void reach_error() { __goblint_check(0); } // FAIL
int __VERIFIER_nondet_int(void);
void ldv___goblint_check(int expression) { if (!expression) { ERROR: {reach_error();abort();}}; return; } // NOWARN

pthread_t t1,t2;

struct device {
	//struct device_private   *p;
};

struct A {
	int a;
	int b;
};

struct my_data {
	pthread_mutex_t lock;
	struct device dev;
	struct A shared;
};

void *my_callback(void *arg) {
	struct device *dev = (struct device*)arg;
	struct my_data *data;
	data = container_of(dev, struct my_data, dev);

	pthread_mutex_lock (&data->lock);
	data->shared.a = 1; // RACE!
	data->shared.b = data->shared.b + 1; // RACE!
	pthread_mutex_unlock (&data->lock);
	return 0;
}

int my_drv_probe(struct my_data *data) {
	struct device *d = &data->dev;

	//init data (single thread)
	//not a race
	pthread_mutex_init(&data->lock, NULL);
	data->shared.a = 0; // NORACE
	data->shared.b = 0; // NORACE

	int res = __VERIFIER_nondet_int();
	if(res)
		goto exit;
	//register callback
	pthread_create(&t1, NULL, my_callback, (void *)d);
	pthread_create(&t2, NULL, my_callback, (void *)d);
	return 0;

exit:
	pthread_mutex_destroy(&data->lock);
	return -1;
}

void my_drv_disconnect(struct my_data *data) {
	void *status;
	pthread_join(t1, &status);
	//race on data->shared.a and data->shared.b
	data->shared.a = 3; // RACE!
	data->shared.b = 3; // RACE!
	ldv___goblint_check(data->shared.a==3); // RACE!
	ldv___goblint_check(data->shared.b==3); // RACE!

	pthread_join(t2, &status);
	pthread_mutex_destroy(&data->lock);
}

int my_drv_init(void) {
	return 0;
}

void my_drv_cleanup(void) {
	return;
}

int main(void) {
	int ret = my_drv_init();
	if(ret==0) {
		int probe_ret;
		struct my_data data;
		probe_ret = my_drv_probe(&data);
		if(probe_ret==0) {
			my_drv_disconnect(&data);
			ldv___goblint_check(data.shared.a==1); // RACE
			ldv___goblint_check(data.shared.b==2); // RACE
		}
		my_drv_cleanup();
		data.shared.a = -1; // RACE
		data.shared.b = -1; // RACE
		ldv___goblint_check(data.shared.a==-1); // RACE
		ldv___goblint_check(data.shared.b==-1); // RACE
	}
	return 0;
}
