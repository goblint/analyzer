// PARAM: --disable sem.lock.fail
#include <pthread.h>

int g_mutex = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

int g_rwlock = 0;
pthread_rwlock_t rwlock = PTHREAD_RWLOCK_INITIALIZER;

// OS X has no spinlock
#ifndef __APPLE__
int g_spin = 0;
pthread_spinlock_t spin;
#endif

void *t_fun(void *arg) {
  if (!pthread_mutex_lock(&mutex)) {
    __goblint_check(1); // reachable
    g_mutex++; // NORACE
    pthread_mutex_unlock(&mutex);
  }
  else {
    __goblint_check(0); // NOWARN (unreachable)
  }

  if (!pthread_mutex_trylock(&mutex)) {
    __goblint_check(1); // reachable
    g_mutex++; // NORACE
    pthread_mutex_unlock(&mutex);
  }
  else {
    __goblint_check(1); // reachable
  }

  if (!pthread_rwlock_wrlock(&mutex)) {
    __goblint_check(1); // reachable
    g_rwlock++; // NORACE
    pthread_rwlock_unlock(&mutex);
  }
  else {
    __goblint_check(0); // NOWARN (unreachable)
  }

  if (!pthread_rwlock_trywrlock(&mutex)) {
    __goblint_check(1); // reachable
    g_rwlock++; // NORACE
    pthread_rwlock_unlock(&mutex);
  }
  else {
    __goblint_check(1); // reachable
  }

  if (!pthread_rwlock_rdlock(&mutex)) {
    __goblint_check(1); // reachable
    g_rwlock++; // NORACE
    pthread_rwlock_unlock(&mutex);
  }
  else {
    __goblint_check(0); // NOWARN (unreachable)
  }

  if (!pthread_rwlock_tryrdlock(&mutex)) {
    __goblint_check(1); // reachable
    g_rwlock++; // NORACE
    pthread_rwlock_unlock(&mutex);
  }
  else {
    __goblint_check(1); // reachable
  }

  int reach1 = 0, reach2 = 0, reach3 = 0;
#ifndef __APPLE__
  if (!pthread_spin_lock(&spin)) {
    reach1 = 1;
    g_spin++; // NORACE
    pthread_spin_unlock(&spin);
  }
  else {
    __goblint_check(0); // NOWARN (unreachable)
  }

  if (!pthread_spin_trylock(&spin)) {
    reach2 = 1;
    g_spin++; // NORACE
    pthread_spin_unlock(&spin);
  }
  else {
    reach3 = 1;
  }
#else
  // fake values so test passes on OSX
  reach1 = 1;
  int r;
  if (r)
    reach2 = 1;
  else
    reach3 = 1;
#endif
  __goblint_check(reach1); // always reached
  __goblint_check(reach2); // UNKNOWN (sometimes reached)
  __goblint_check(reach3); // UNKNOWN (sometimes reached)

  return NULL;
}

int main() {
#ifndef __APPLE__
  pthread_spin_init(&spin, PTHREAD_PROCESS_PRIVATE);
#endif

  pthread_t id;
  pthread_create(&id, NULL, &t_fun, NULL);

  pthread_mutex_lock(&mutex);
  g_mutex++; // NORACE
  pthread_mutex_unlock(&mutex);

  pthread_rwlock_wrlock(&mutex);
  g_rwlock++; // NORACE
  pthread_rwlock_unlock(&mutex);

#ifndef __APPLE__
  pthread_spin_lock(&spin);
  g_spin++; // NORACE
  pthread_spin_unlock(&spin);
#endif

  pthread_join(id, NULL);
  return 0;
}
