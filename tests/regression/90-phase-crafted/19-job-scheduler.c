// CRAM
// Job scheduler has dispatch, retry, retire, and priority phases.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t sched_lock;
struct Scheduler { int queued; int running; int retired; int priority_mask; } scheduler;

void *dispatcher(void *arg) {
  int n = 0;
  for (int lane = 0; lane < 4; lane++)
    n += lane < 3;
  pthread_mutex_lock(&sched_lock);
  /* GHOST dispatcher 1 */ scheduler.queued -= n;
  pthread_mutex_unlock(&sched_lock);
  pthread_mutex_lock(&sched_lock);
  /* GHOST dispatcher 2 */ scheduler.running += n;
  pthread_mutex_unlock(&sched_lock);
  return 0;
}

void *retirer(void *arg) {
  int done = 0;
  while (done < 2)
    done++;
  pthread_mutex_lock(&sched_lock);
  /* GHOST retirer 1 */ scheduler.running -= done;
  pthread_mutex_unlock(&sched_lock);
  pthread_mutex_lock(&sched_lock);
  /* GHOST retirer 2 */ scheduler.retired += done;
  pthread_mutex_unlock(&sched_lock);
  return 0;
}

void *prioritizer(void *arg) {
  pthread_mutex_lock(&sched_lock);
  /* GHOST prioritizer 1 */ scheduler.priority_mask |= 5;
  pthread_mutex_unlock(&sched_lock);
  return 0;
}

int main(void) {
  pthread_t d, r, p;
  pthread_mutex_init(&sched_lock, 0);
  scheduler.queued = 9;
  scheduler.running = 4;
  scheduler.retired = 20;
  scheduler.priority_mask = 2;
  pthread_create(&d, 0, dispatcher, 0);
  pthread_create(&r, 0, retirer, 0);
  pthread_create(&p, 0, prioritizer, 0);
  pthread_join(d, 0);
  pthread_join(r, 0);
  pthread_join(p, 0);
  if (scheduler.queued != 6 || scheduler.running != 5 || scheduler.retired != 22 || scheduler.priority_mask != 7) {
    reach_error();
    abort();
  }
  return 0;
}
