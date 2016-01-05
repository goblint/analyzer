// PARAM: --sets ana.activated[+] deadlock
#include <pthread.h>
#include <stdio.h>

typedef struct {
  int balance;
  int id;
  pthread_mutex_t mutex;
} bank_account;

bank_account A, B;

int counter = 0;

init_account(bank_account *a) {
  a->id = counter++;
  a->balance = 0;
  pthread_mutex_init(&a->mutex, NULL);
}

void deposit(bank_account *f, bank_account *t, int ammount) {
  if (f->id == t->id)
    return;

  if (f->id < t->id) {
    pthread_mutex_lock(&f->mutex);
    pthread_mutex_lock(&t->mutex); // DEADLOCK
  } else {
    pthread_mutex_lock(&f->mutex);
    pthread_mutex_lock(&t->mutex); // DEADLOCK
  }

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
  deposit(&B, &A, rand() % 100);
  return NULL;
}

int main(void) {
  pthread_t id1, id2;
  init_account(&A);
  init_account(&B);
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
