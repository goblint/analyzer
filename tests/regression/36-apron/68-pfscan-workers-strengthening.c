// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet --set sem.int.signed_overflow assume_none --enable ana.apron.strengthening
// minimized pfscan with relational workers invariant
// mutex-meet: needs strengthening even with path_sens threadflag
// mutex-meet-tid: doesn't need strengthening
// needs assume_none to avoid top via some lost upper bounds
#include <assert.h>
#include <pthread.h>

struct __anonstruct_PQUEUE_63 {
   pthread_mutex_t mtx ;
   pthread_cond_t less ;
};
typedef struct __anonstruct_PQUEUE_63 PQUEUE;

int nworkers  =    0;
int aworkers  =    0;
pthread_mutex_t aworker_lock = PTHREAD_MUTEX_INITIALIZER;
PQUEUE pqb  ;

void *worker(void *arg)
{
  pthread_mutex_lock(& aworker_lock);
  aworkers --;
  assert(aworkers <= nworkers);
  pthread_mutex_unlock(& aworker_lock);
  return NULL;
}

int main()
{
  int r ; // rand
  pthread_t tid ;
  nworkers = r;
  aworkers = nworkers;

  for (int j = 0; j < nworkers; j++)
    pthread_create(& tid, NULL, & worker, NULL);

  while (1) {
    pthread_cond_wait(&pqb.less, &pqb.mtx); // some weirdness that exposes the problem
  }

  return 0;
}
