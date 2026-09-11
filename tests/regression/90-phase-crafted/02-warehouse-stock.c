// CRAM
// Warehouse receiving, picking, and cycle-count correction with different phase counts.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t stock_lock;
struct Stock { int on_hand; int reserved; int damaged; } stock;

void *receiver(void *arg) {
  int crates = 0;
  for (int i = 0; i < 3; i++)
    crates += 2;
  pthread_mutex_lock(&stock_lock);
  /* GHOST receiver 1 */ stock.on_hand += crates;
  pthread_mutex_unlock(&stock_lock);
  return 0;
}

void *picker(void *arg) {
  int picked = 0;
  for (int n = 3; n > 0; n--)
    picked += n;
  int packed = 1;
  for (int i = 0; i < 2; i++)
    packed *= 2;
  pthread_mutex_lock(&stock_lock);
  /* GHOST picker 1 */ stock.on_hand -= picked;
  pthread_mutex_unlock(&stock_lock);
  pthread_mutex_lock(&stock_lock);
  /* GHOST picker 2 */ stock.reserved -= packed;
  pthread_mutex_unlock(&stock_lock);
  return 0;
}

void *cycle_counter(void *arg) {
  int found = 0;
  for (int shelf = 0; shelf < 4; shelf++)
    if (shelf != 2)
      found++;
  pthread_mutex_lock(&stock_lock);
  /* GHOST cycle_counter 1 */ stock.damaged += found;
  pthread_mutex_unlock(&stock_lock);
  return 0;
}

int main(void) {
  pthread_t r, p, c;
  pthread_mutex_init(&stock_lock, 0);
  stock.on_hand = 30;
  stock.reserved = 10;
  stock.damaged = 1;
  pthread_create(&r, 0, receiver, 0);
  pthread_create(&p, 0, picker, 0);
  pthread_create(&c, 0, cycle_counter, 0);
  pthread_join(r, 0);
  pthread_join(p, 0);
  pthread_join(c, 0);
  if (stock.on_hand < 30 || stock.on_hand > 30 || stock.reserved != 6 || stock.damaged != 4) {
    reach_error();
    abort();
  }
  return 0;
}
