// CRAM
// Telemetry rollup uses array slots, parity, and a sanity relation instead of one counter.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

enum { OK, DROPPED, PARITY, N };
pthread_mutex_t telemetry_lock;
int telemetry[N];

void *sampler(void *arg) {
  int ok = 0;
  for (int code = 2; code <= 8; code += 2)
    ok += code / 2;
  int parity = 0;
  for (int i = 0; i < 5; i++)
    parity ^= (i & 1);
  pthread_mutex_lock(&telemetry_lock);
  /* GHOST sampler 1 */ telemetry[OK] += ok;
  pthread_mutex_unlock(&telemetry_lock);
  pthread_mutex_lock(&telemetry_lock);
  /* GHOST sampler 2 */ telemetry[PARITY] ^= parity;
  pthread_mutex_unlock(&telemetry_lock);
  return 0;
}

void *radio(void *arg) {
  int dropped = 0;
  for (int retry = 0; retry < 6; retry++)
    dropped += (retry == 1 || retry == 4);
  pthread_mutex_lock(&telemetry_lock);
  /* GHOST radio 1 */ telemetry[DROPPED] += dropped;
  pthread_mutex_unlock(&telemetry_lock);
  return 0;
}

int main(void) {
  pthread_t s, r;
  pthread_mutex_init(&telemetry_lock, 0);
  telemetry[OK] = 100;
  telemetry[DROPPED] = 3;
  telemetry[PARITY] = 1;
  pthread_create(&s, 0, sampler, 0);
  pthread_create(&r, 0, radio, 0);
  pthread_join(s, 0);
  pthread_join(r, 0);
  if ((telemetry[OK] - telemetry[DROPPED]) != 105 || telemetry[PARITY] != 1) {
    reach_error();
    abort();
  }
  return 0;
}
