#include <pthread.h>

struct __anonstruct_PQUEUE_63 {
   int closed ;
   pthread_mutex_t mtx ;
};
typedef struct __anonstruct_PQUEUE_63 PQUEUE;

PQUEUE pqb;

int pqueue_init(PQUEUE *qp)
{
  qp->closed = 0;
  pthread_mutex_init(& qp->mtx, NULL);
  return (0);
}

void pqueue_close(PQUEUE *qp )
{
  pthread_mutex_lock(& qp->mtx);
  qp->closed = 1;
  pthread_mutex_unlock(& qp->mtx);
  return;
}

int pqueue_put(PQUEUE *qp)
{
  pthread_mutex_lock(& qp->mtx);
  if (qp->closed) {
    // pfscan actually has a bug and is missing the following unlock at early return
    // pthread_mutex_unlock(& qp->mtx);

    return (0);
  }
  pthread_mutex_unlock(& qp->mtx);
  return (1);
}

void *worker(void *arg )
{
  return NULL;
}

int main(int argc , char **argv )
{
  pthread_t tid;

  PQUEUE *qp = &pqb;
  pqueue_init(& pqb);
  pthread_create(& tid, NULL, & worker, NULL);

  for (int i = 1; i < argc; i++) {
    pqueue_put(& pqb);
  }

  pqueue_close(& pqb);
  return 0;
}
