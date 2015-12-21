// PARAM: --sets ana.activated[+] deadlock
#include <pthread.h>
#include <stdio.h>

typedef struct {
  int balance;
  pthread_mutex_t mutex;
} bank_account;

bank_account A, B;

void deposit(bank_account *f, bank_account *t, int ammount) {
  pthread_mutex_lock(&f->mutex);
  pthread_mutex_lock(&t->mutex);  // NODEADLOCK
  t->balance += ammount;
  f->balance -= ammount;
  pthread_mutex_unlock(&t->mutex);
  pthread_mutex_unlock(&f->mutex);
}


void *t1(void *arg) {
  deposit(&A, &B, rand() % 100);
  return NULL;
}

void *t2(void *arg) {
  deposit(&A, &B, -(rand() % 100));
  return NULL;
}

int main(void) {
  pthread_t id1, id2;
  pthread_mutex_init(&A.mutex, NULL);
  pthread_mutex_init(&B.mutex, NULL);
  int i;
  for (i = 0; i < 100000; i++) {
    pthread_create(&id1, NULL, t1, NULL);
    pthread_create(&id2, NULL, t2, NULL);
    pthread_join (id1, NULL);
    pthread_join (id2, NULL);
    printf("%d: A = %d, B = %d.\n", i, A.balance, B.balance);
  }
  return 0;
}
