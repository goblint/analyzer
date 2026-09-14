// CRAM
// Audit aggregation combines range checks, xor fingerprints, and alert accounting.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t audit_lock;
struct Audit { int checked; int alerts; int fingerprint; } audit;

void *scanner(void *arg) {
  int checked = 0;
  for (int v = 1; v <= 6; v++)
    checked += (v % 3) != 0;
  pthread_mutex_lock(&audit_lock);
  /* GHOST scanner 1 */ audit.checked += checked;
  pthread_mutex_unlock(&audit_lock);
  pthread_mutex_lock(&audit_lock);
  /* GHOST scanner 2 */ audit.fingerprint ^= 12;
  pthread_mutex_unlock(&audit_lock);
  return 0;
}

void *alerter(void *arg) {
  int alerts = 0;
  for (int i = 0; i < 5; i++)
    alerts += (i == 0 || i == 4);
  pthread_mutex_lock(&audit_lock);
  /* GHOST alerter 1 */ audit.alerts += alerts;
  pthread_mutex_unlock(&audit_lock);
  return 0;
}

void *deduper(void *arg) {
  pthread_mutex_lock(&audit_lock);
  /* GHOST deduper 1 */ audit.alerts -= 1;
  pthread_mutex_unlock(&audit_lock);
  return 0;
}

int main(void) {
  pthread_t s, a, d;
  pthread_mutex_init(&audit_lock, 0);
  audit.checked = 96;
  audit.alerts = 10;
  audit.fingerprint = 3;
  pthread_create(&s, 0, scanner, 0);
  pthread_create(&a, 0, alerter, 0);
  pthread_create(&d, 0, deduper, 0);
  pthread_join(s, 0);
  pthread_join(a, 0);
  pthread_join(d, 0);
  if (audit.checked != 100 || audit.alerts != 11 || audit.fingerprint != 15) {
    reach_error();
    abort();
  }
  return 0;
}
