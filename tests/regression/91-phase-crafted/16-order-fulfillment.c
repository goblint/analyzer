// CRAM
// Order fulfillment: richer helpers, but only a handful of shared phases.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

enum Shipping { STANDARD, EXPRESS, COLD_CHAIN };
struct Order { int units; int fragile; enum Shipping shipping; };
struct Fulfillment { int picked; int packed; int labels; } fulfillment;
pthread_mutex_t fulfillment_lock;

static int pick_units(struct Order o) {
  int rounds = 0;
  for (int remaining = o.units; remaining > 0; remaining -= 2)
    rounds += remaining > 1 ? 2 : 1;
  return rounds;
}

static int label_count(struct Order o) {
  int labels = 1;
  if (o.shipping == EXPRESS)
    labels += 1;
  if (o.shipping == COLD_CHAIN)
    labels += 2;
  for (int i = 0; i < o.fragile; i++)
    labels += (i == 0);
  return labels;
}

static int valid_order(struct Order o) {
  if (o.units <= 0)
    return 0;
  if (o.fragile > o.units)
    return 0;
  return 1;
}

void *picker_thread(void *arg) {
  struct Order o = {5, 1, EXPRESS};
  int picked = 0;
  if (o.units > 0 && o.fragile <= o.units)
    for (int remaining = o.units; remaining > 0; remaining -= 2)
      picked += remaining > 1 ? 2 : 1;
  pthread_mutex_lock(&fulfillment_lock);
  /* GHOST picker_thread 1 */ fulfillment.picked += picked;
  pthread_mutex_unlock(&fulfillment_lock);
  return 0;
}

void *packer_thread(void *arg) {
  int packed = 0;
  for (int station = 0; station < 3; station++)
    packed += station + 1;
  pthread_mutex_lock(&fulfillment_lock);
  /* GHOST packer_thread 1 */ fulfillment.packed += packed;
  pthread_mutex_unlock(&fulfillment_lock);
  pthread_mutex_lock(&fulfillment_lock);
  int labels = 1;
  labels += 2;
  /* GHOST packer_thread 2 */ fulfillment.labels += labels;
  pthread_mutex_unlock(&fulfillment_lock);
  return 0;
}

int main(void) {
  pthread_t p, k;
  pthread_mutex_init(&fulfillment_lock, 0);
  fulfillment.picked = 12;
  fulfillment.packed = 3;
  fulfillment.labels = 4;
  pthread_create(&p, 0, picker_thread, 0);
  pthread_create(&k, 0, packer_thread, 0);
  pthread_join(p, 0);
  pthread_join(k, 0);
  if (fulfillment.picked != 17 || fulfillment.packed != 9 || fulfillment.labels != 7) {
    reach_error();
    abort();
  }
  return 0;
}
