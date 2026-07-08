// CRAM
// mixed window table: table update protocol with mixed arithmetic, ranges, and masks.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t table_lock;
int table[4];
int mode;

void *alpha(void *arg) {
  int s = 0;
  for (int i = 0; i < 4; i++)
    s += (i + 0);
  pthread_mutex_lock(&table_lock);
  /* GHOST alpha 1 */ table[0] += s;
  pthread_mutex_unlock(&table_lock);
  pthread_mutex_lock(&table_lock);
  /* GHOST alpha 2 */ mode |= 1;
  pthread_mutex_unlock(&table_lock);
  return 0;
}

void *beta(void *arg) {
  int s = 1;
  for (int i = 0; i < 3; i++)
    s *= 2;
  pthread_mutex_lock(&table_lock);
  /* GHOST beta 1 */ table[1] += s;
  pthread_mutex_unlock(&table_lock);
  return 0;
}

void *gamma(void *arg) {
  int s = 0;
  while (s < 2)
    s++;
  pthread_mutex_lock(&table_lock);
  /* GHOST gamma 1 */ table[2] -= s;
  pthread_mutex_unlock(&table_lock);
  return 0;
}

int main(void) {
  pthread_t a, b, g;
  pthread_mutex_init(&table_lock, 0);
  table[0] = 48;
  table[1] = 53;
  table[2] = 58;
  table[3] = 99;
  mode = 0;
  pthread_create(&a, 0, alpha, 0);
  pthread_create(&b, 0, beta, 0);
  pthread_create(&g, 0, gamma, 0);
  pthread_join(a, 0);
  pthread_join(b, 0);
  pthread_join(g, 0);
  if (table[0] != 54 || table[1] != 61 || table[2] != 56 || mode == 0) {
    reach_error();
    abort();
  }
  return 0;
}
