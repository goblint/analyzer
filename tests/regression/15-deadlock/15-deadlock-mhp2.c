// PARAM: --set ana.activated[+] deadlock --set ana.activated[+] threadJoins
#include <pthread.h>

pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m3 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m4 = PTHREAD_MUTEX_INITIALIZER;
pthread_t decoy;


void *noOpThread() {
  return NULL;
}

void *thread2() {
  pthread_mutex_lock(&m3); // NODEADLOCK (decoy not yet created)
  pthread_mutex_lock(&m1); // NODEADLOCK (decoy not yet created)

  pthread_mutex_unlock(&m3);
  pthread_mutex_unlock(&m1);

  return NULL;
}

void *thread() {
  pthread_mutex_lock(&m2); // NODEADLOCK (decoy not yet created)
  pthread_mutex_lock(&m3); // NODEADLOCK (decoy not yet created)

  pthread_mutex_lock(&m4); // NODEADLOCK (not in cycle)
  pthread_create(&decoy, NULL, noOpThread, NULL);
  pthread_mutex_unlock(&m4);


  pthread_mutex_unlock(&m3);
  pthread_mutex_unlock(&m2);

  return NULL;
}

int main() {
  pthread_t tid1;
  pthread_t tid2;

  pthread_create(&tid1, NULL, thread, NULL);
  pthread_create(&tid2, NULL, thread2, NULL);

  pthread_mutex_lock(&m1); // NODEADLOCK (decoy not yet created)

  pthread_mutex_lock(&m4); // NODEADLOCK (not in cycle)
  pthread_t dec = decoy;
  pthread_mutex_unlock(&m4);

  if(dec != NULL) {
    pthread_join(dec,NULL);
    pthread_mutex_lock(&m2); // NODEADLOCK (decoy not yet created)

    pthread_mutex_unlock(&m1);
    pthread_mutex_unlock(&m2);
  } else {
    pthread_mutex_unlock(&m1);
  }

  return 0;
}
