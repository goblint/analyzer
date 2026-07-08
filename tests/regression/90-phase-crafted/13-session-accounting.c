// CRAM
// Session accounting toggles booleans and tracks a nonce through three phases.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t session_lock;
struct Session { int encrypted; int compressed; int nonce; } session;

static int nonce_delta(void) {
  int delta = 0;
  for (int round = 0; round < 4; round++)
    delta ^= (round + 1);
  return delta;
}

void *tls_worker(void *arg) {
  pthread_mutex_lock(&session_lock);
  /* GHOST tls_worker 1 */ session.encrypted = !session.encrypted;
  pthread_mutex_unlock(&session_lock);
  pthread_mutex_lock(&session_lock);
  /* GHOST tls_worker 2 */ session.nonce ^= nonce_delta();
  pthread_mutex_unlock(&session_lock);
  return 0;
}

void *zip_worker(void *arg) {
  for (int once = 0; once < 1; once++) {
    pthread_mutex_lock(&session_lock);
    /* GHOST zip_worker 1 */ session.compressed = !session.compressed;
    pthread_mutex_unlock(&session_lock);
  }
  return 0;
}

void *audit_worker(void *arg) {
  pthread_mutex_lock(&session_lock);
  /* GHOST audit_worker 1 */ session.nonce += 7;
  pthread_mutex_unlock(&session_lock);
  return 0;
}

int main(void) {
  pthread_t t, z, a;
  pthread_mutex_init(&session_lock, 0);
  session.encrypted = 0;
  session.compressed = 1;
  session.nonce = 3;
  pthread_create(&t, 0, tls_worker, 0);
  pthread_create(&z, 0, zip_worker, 0);
  pthread_create(&a, 0, audit_worker, 0);
  pthread_join(t, 0);
  pthread_join(z, 0);
  pthread_join(a, 0);
  if (!session.encrypted || session.compressed || session.nonce != 14) {
    reach_error();
    abort();
  }
  return 0;
}
