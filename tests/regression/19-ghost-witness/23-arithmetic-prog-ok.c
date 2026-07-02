// CRAM
// This file is part of the SV-Benchmarks collection of verification tasks:
// https://github.com/sosy-lab/sv-benchmarks
//
// SPDX-FileCopyrightText: 2016 SCTBench Project
// SPDX-FileCopyrightText: The ESBMC Project
//
// SPDX-License-Identifier: Apache-2.0
// https://github.com/mc-imperial/sctbench/blob/master/benchmarks/concurrent-software-benchmarks/arithmetic_prog_ok.c
#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#define N 4

extern void abort(void);
void reach_error(void) { }
void __VERIFIER_assert(int cond) { if (!cond) { reach_error(); abort(); } }

int num;
unsigned long total;
int flag;
pthread_mutex_t m;
pthread_cond_t empty, full;

void *thread1(void *arg) {
  int i;
  i = 0;
  while (i < N) {
    pthread_mutex_lock(&m);
    while (num > 0)
      pthread_cond_wait(&empty, &m);
    num++;
    pthread_mutex_unlock(&m);
    pthread_cond_signal(&full);
    i++;
  }
}

void *thread2(void *arg) {
  int j;
  j = 0;
  while (j < N) {
    pthread_mutex_lock(&m);
    while (num == 0)
      pthread_cond_wait(&full, &m);
    total = total + j;
    num--;
    pthread_mutex_unlock(&m);
    pthread_cond_signal(&empty);
    j++;
  }
  total = total + j;
  flag = 1;
}

int main(void) {
  pthread_t t1, t2;

  num = 0;
  total = 0;
  pthread_mutex_init(&m, 0);
  pthread_cond_init(&empty, 0);
  pthread_cond_init(&full, 0);
  pthread_create(&t1, 0, thread1, 0);
  pthread_create(&t2, 0, thread2, 0);
  pthread_join(t1, 0);
  pthread_join(t2, 0);
  if (flag)
    __VERIFIER_assert(total == ((N * (N + 1)) / 2));
  return 0;
}
