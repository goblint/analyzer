#include <pthread.h>
#include <assert.h>

pthread_mutex_t __global_lock = PTHREAD_MUTEX_INITIALIZER;

#define inc(x) { pthread_mutex_lock(&__global_lock); (x)++; pthread_mutex_unlock(&__global_lock); }
#define dec(x) { pthread_mutex_lock(&__global_lock); (x)--; pthread_mutex_unlock(&__global_lock); }

#define access(x) { inc(x); dec(x); }

#define assert_racefree(x) { pthread_mutex_lock(&__global_lock); assert((x) == 0); pthread_mutex_unlock(&__global_lock); }

