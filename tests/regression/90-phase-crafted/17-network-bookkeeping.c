// CRAM
// Network bookkeeping scales windows and rotates masks rather than simply incrementing.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t net_lock;
struct Net { int tx_window; int rx_window; int mask; } net;

void *sender(void *arg) {
  int m = 1;
  for (int i = 0; i < 2; i++)
    m *= 2;
  pthread_mutex_lock(&net_lock);
  /* GHOST sender 1 */ net.tx_window *= m;
  pthread_mutex_unlock(&net_lock);
  pthread_mutex_lock(&net_lock);
  /* GHOST sender 2 */ net.mask |= 8;
  pthread_mutex_unlock(&net_lock);
  return 0;
}

void *receiver(void *arg) {
  int adjust = 0;
  for (int i = 0; i < 5; i++)
    adjust += (i & 1);
  pthread_mutex_lock(&net_lock);
  /* GHOST receiver 1 */ net.rx_window += adjust;
  pthread_mutex_unlock(&net_lock);
  return 0;
}

int main(void) {
  pthread_t s, r;
  pthread_mutex_init(&net_lock, 0);
  net.tx_window = 10;
  net.rx_window = 7;
  net.mask = 3;
  pthread_create(&s, 0, sender, 0);
  pthread_create(&r, 0, receiver, 0);
  pthread_join(s, 0);
  pthread_join(r, 0);
  if (net.tx_window != 40 || net.rx_window != 9 || (net.mask & 11) != 11) {
    reach_error();
    abort();
  }
  return 0;
}
