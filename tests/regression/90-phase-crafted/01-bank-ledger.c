// CRAM
// Two bank workers reconcile a ledger using loop-computed posting batches.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t ledger_lock;
struct Ledger { int deposits; int withdrawals; int flags; } ledger;

static int sum_table(const int *xs, int n) {
  int r = 0;
  for (int i = 0; i < n; i++)
    r += xs[i];
  return r;
}

void *card_settlement(void *arg) {
  int captured[] = {3, 4, 5};
  int rejected = 0;
  for (int i = 0; i < 4; i++)
    rejected += (i % 2);
  pthread_mutex_lock(&ledger_lock);
  /* GHOST card_settlement 1 */ ledger.deposits += sum_table(captured, 3);
  pthread_mutex_unlock(&ledger_lock);
  pthread_mutex_lock(&ledger_lock);
  /* GHOST card_settlement 2 */ ledger.withdrawals += rejected;
  pthread_mutex_unlock(&ledger_lock);
  return 0;
}

void *ach_settlement(void *arg) {
  int batches[] = {7, 2};
  int mask = 0;
  for (int i = 0; i < 3; i++)
    mask ^= (1 << i);
  pthread_mutex_lock(&ledger_lock);
  /* GHOST ach_settlement 1 */ ledger.deposits += sum_table(batches, 2);
  pthread_mutex_unlock(&ledger_lock);
  pthread_mutex_lock(&ledger_lock);
  /* GHOST ach_settlement 2 */ ledger.flags ^= mask;
  pthread_mutex_unlock(&ledger_lock);
  return 0;
}

int main(void) {
  pthread_t a, b;
  pthread_mutex_init(&ledger_lock, 0);
  ledger.deposits = 20;
  ledger.withdrawals = 5;
  ledger.flags = 1;
  pthread_create(&a, 0, card_settlement, 0);
  pthread_create(&b, 0, ach_settlement, 0);
  pthread_join(a, 0);
  pthread_join(b, 0);
  if (ledger.flags != 6) {
    reach_error();
    abort();
  }
  return 0;
}
