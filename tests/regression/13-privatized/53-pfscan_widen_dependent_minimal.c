// PARAM: --enable ana.int.interval --enable exp.priv-distr-init
#include <pthread.h>
#include <assert.h>

// protection priv succeeds
// global-history fails due to [1,1] widen [0,1] -> [-inf,1]
// sensitive to eval and widen order!

struct __anonstruct_PQUEUE_63 {
   int qsize ;
   int occupied ;
   pthread_mutex_t mtx ;
};
typedef struct __anonstruct_PQUEUE_63 PQUEUE;

PQUEUE pqb  ;

int pqueue_init(PQUEUE *qp , int qsize )
{
  qp->qsize = qsize;
  qp->occupied = 0;
  pthread_mutex_init(& qp->mtx, NULL);
  return (0);
}

int pqueue_put(PQUEUE *qp)
{
  pthread_mutex_lock(& qp->mtx);
  while (qp->occupied >= qp->qsize) {

  }
  assert(qp->occupied >= 0); // TODO
  (qp->occupied) ++;
  pthread_mutex_unlock(& qp->mtx);
  return (1);
}

int pqueue_get(PQUEUE *qp)
{
  int got = 0;
  pthread_mutex_lock(& qp->mtx);
  while (qp->occupied <= 0) {

  }
  assert(qp->occupied > 0); // TODO
  if (qp->occupied > 0) {
    (qp->occupied) --;
    got = 1;
    pthread_mutex_unlock(& qp->mtx);
  } else {
    pthread_mutex_unlock(& qp->mtx);
  }
  return (got);
}


void *worker(void *arg )
{
  while (1) {
    pqueue_get(& pqb);
  }
  return NULL;
}

int main(int argc , char **argv )
{
  pthread_t tid;
  int qsize;

  PQUEUE *qp = &pqb;
  pqueue_init(& pqb, qsize);
  pthread_create(& tid, NULL, & worker, NULL);

  for (int i = 1; i < argc; i++) {
    pqueue_put(& pqb);
  }
  return 0;
}
