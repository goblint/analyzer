// CRAM
// Payment batch: large-ish real-world scaffolding, only two phase boundaries matter.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

enum Channel { CARD, WALLET, BANK_TRANSFER };
struct Payment { int amount; int risk; enum Channel channel; int retries; };
struct Totals { int authorized; int captured; int chargebacks; } totals;
pthread_mutex_t totals_lock;

static int channel_limit(enum Channel c) {
  switch (c) {
    case CARD: return 80;
    case WALLET: return 35;
    default: return 120;
  }
}

static int score_payment(struct Payment p) {
  int score = 0;
  for (int i = 0; i <= p.retries; i++)
    score += p.risk + i;
  if (p.amount > channel_limit(p.channel))
    score += 100;
  return score;
}

static int should_authorize(struct Payment p) {
  if (p.amount <= 0)
    return 0;
  if (score_payment(p) >= 50)
    return 0;
  return 1;
}

void *authorizer(void *arg) {
  struct Payment batch[] = {{20, 3, CARD, 1}, {15, 2, WALLET, 2}, {25, 1, BANK_TRANSFER, 0}};
  int accepted = 0;
  for (int i = 0; i < 3; i++) {
    int score = 0;
    for (int retry = 0; retry <= batch[i].retries; retry++)
      score += batch[i].risk + retry;
    int limit = 120;
    if (batch[i].channel == CARD)
      limit = 80;
    if (batch[i].channel == WALLET)
      limit = 35;
    if (batch[i].amount > limit)
      score += 100;
    if (batch[i].amount > 0 && score < 50)
      accepted += batch[i].amount;
  }
  pthread_mutex_lock(&totals_lock);
  /* GHOST authorizer 1 */ totals.authorized += accepted;
  pthread_mutex_unlock(&totals_lock);
  return 0;
}

void *capturer(void *arg) {
  int captures = 0;
  for (int attempt = 0; attempt < 4; attempt++)
    captures += (attempt + 1) * 2;
  pthread_mutex_lock(&totals_lock);
  /* GHOST capturer 1 */ totals.captured += captures;
  pthread_mutex_unlock(&totals_lock);
  pthread_mutex_lock(&totals_lock);
  /* GHOST capturer 2 */ totals.chargebacks += 1;
  pthread_mutex_unlock(&totals_lock);
  return 0;
}

int main(void) {
  pthread_t a, c;
  pthread_mutex_init(&totals_lock, 0);
  totals.authorized = 10;
  totals.captured = 7;
  totals.chargebacks = 0;
  pthread_create(&a, 0, authorizer, 0);
  pthread_create(&c, 0, capturer, 0);
  pthread_join(a, 0);
  pthread_join(c, 0);
  if (totals.authorized != 70 || totals.captured != 27 || totals.chargebacks > 1) {
    reach_error();
    abort();
  }
  return 0;
}
