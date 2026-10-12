// CRAM
// large security scan: larger benchmark body with few phase boundaries; intended for level00.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

struct Item { int id; int weight; int enabled; };
struct Summary { int accepted; int rejected; int checksum; int spare[8]; } summary;
pthread_mutex_t summary_lock;

static void prepare_items(struct Item *items, int n) {
  for (int i = 0; i < n; i++) {
    items[i].id = i + 1;
    items[i].weight = (i % 3) + 1;
    items[i].enabled = (i != 2);
  }
}

static int local_score(struct Item *items, int n) {
  int score = 0;
  for (int i = 0; i < n; i++) {
    if (items[i].enabled)
      score += items[i].id * items[i].weight;
    else
      score -= items[i].weight;
  }
  return score;
}

void *planner(void *arg) {
  struct Item items[6];
  prepare_items(items, 6);
  int score = 5;
  pthread_mutex_lock(&summary_lock);
  /* GHOST planner 1 */ summary.accepted += score + 7;
  pthread_mutex_unlock(&summary_lock);
  return 0;
}

void *auditor(void *arg) {
  struct Item items[5];
  prepare_items(items, 5);
  int checksum = 21;
  int rejected = 1;
  pthread_mutex_lock(&summary_lock);
  /* GHOST auditor 1 */ summary.rejected += rejected + 7;
  pthread_mutex_unlock(&summary_lock);
  pthread_mutex_lock(&summary_lock);
  /* GHOST auditor 2 */ summary.checksum ^= checksum;
  pthread_mutex_unlock(&summary_lock);
  return 0;
}

int main(void) {
  pthread_t p, a;
  pthread_mutex_init(&summary_lock, 0);
  summary.accepted = 139;
  summary.rejected = 3;
  summary.checksum = 3;
  for (int i = 0; i < 8; i++)
    summary.spare[i] = i;
  pthread_create(&p, 0, planner, 0);
  pthread_create(&a, 0, auditor, 0);
  pthread_join(p, 0);
  pthread_join(a, 0);
  if (summary.accepted != 151 || summary.rejected != 11 || summary.checksum != 22) {
    reach_error();
    abort();
  }
  return 0;
}
