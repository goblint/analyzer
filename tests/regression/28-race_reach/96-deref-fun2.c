// PARAM: --enable ana.sv-comp.enabled --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )"
#include<pthread.h>

#define SVCOMP 1

#include <pthread.h>
#include <assert.h>

extern void abort(void);


#define N 10000
#define create_threads(t) pthread_t t##_ids[N]; for (int i=0; i<N; i++) pthread_create(&t##_ids[i], NULL, t##_fun, NULL)
#define join_threads(t) for (int i=0; i < N; i++) pthread_join (t##_ids[i], NULL)



int global;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t * mp = &mutex1;

void *t_fun(void *arg) {
  pthread_mutex_t * mp1;
  int *p = &global;
  mp1 = mp;
  pthread_mutex_lock(mp);

  pthread_mutex_unlock(mp);
  return NULL;
}

int main(void) {
  pthread_mutex_t * mp1;
  mp = &mutex1;
  create_threads(t);
  mp1 = mp;
  pthread_mutex_lock(mp);

  pthread_mutex_unlock(mp);
  join_threads(t);
  return 0;
}
