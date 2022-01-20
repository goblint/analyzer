// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --sets ana.apron.privatization mutex-meet-tid --disable ana.thread.include-node
#include <pthread.h>
#include <assert.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_benign(void *arg) {
  pthread_mutex_lock(&A);
  g = 20;
  pthread_mutex_unlock(&A);
  return NULL;
}

void *u_benign(void *arg) {
  pthread_mutex_lock(&A);
  h = 20;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int t;

  pthread_t id;

  if(t) {
    pthread_create(&id, NULL, t_benign, NULL);
  } else {
    pthread_create(&id, NULL, t_benign, NULL);
  }

  // As these two threads are not distinguished, we have a unique TID for id
  pthread_join(id, NULL);


  pthread_mutex_lock(&A);
  g = 12;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  assert(g == 12);
  pthread_mutex_unlock(&A);

// ---------------------------------------------------------------------------

  pthread_t id2;
  pthread_t id3;


  pthread_create(&id2, NULL, u_benign, NULL);
  pthread_create(&id3, NULL, u_benign, NULL);

  pthread_join(id2, NULL);

  // As these two threads are not distinguished, id3 is a not unique thread
  pthread_join(id3, NULL);


  pthread_mutex_lock(&A);
  h = 12;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  assert(h == 12); //TODO
  pthread_mutex_unlock(&A);

  return 0;
}
