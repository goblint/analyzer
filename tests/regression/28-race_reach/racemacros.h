#define SVCOMP 0

#include <pthread.h>
#include <assert.h>

#if SVCOMP
extern void abort(void);
void reach_error() { assert(0); }
void __VERIFIER_assert(int cond) { if(!(cond)) { ERROR: {reach_error();abort();} } }
#else
#define __VERIFIER_assert assert
#endif
extern int __VERIFIER_nondet_int();

pthread_mutex_t __global_lock = PTHREAD_MUTEX_INITIALIZER;

#define inc(x) do { pthread_mutex_lock(&__global_lock); (x)++; pthread_mutex_unlock(&__global_lock); } while (0)
#define dec(x) do { pthread_mutex_lock(&__global_lock); (x)--; pthread_mutex_unlock(&__global_lock); } while (0)

#define access(x) do { inc(x); dec(x); } while (0)

#define assert_racefree(x) do { pthread_mutex_lock(&__global_lock); __VERIFIER_assert((x) == 0); pthread_mutex_unlock(&__global_lock); } while (0)

#define access_or_assert_racefree(x) do { if (__VERIFIER_nondet_int()) access(x); else assert_racefree(x); } while (0)

#define N 10000
#define create_threads(t) pthread_t t##_ids[N]; for (int i=0; i<N; i++) pthread_create(&t##_ids[i], NULL, t##_fun, NULL)
#define join_threads(t) for (int i=0; i < N; i++) pthread_join (t##_ids[i], NULL)
