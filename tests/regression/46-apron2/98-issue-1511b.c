#include<pthread.h>
int d, j, k;

pthread_mutex_t f;

void nothing() {};
void nothing2() {};

int top() {
  int top2;
  return top2;
}

void main() {
  d = top();
  if (d) {
    k = 1;
    pthread_t tid;
    pthread_create(&tid, 0, &nothing, NULL);
    pthread_mutex_lock(&f);
    j = 0; // To ensure something is published
    pthread_mutex_unlock(&f);
    pthread_mutex_lock(&f);

    pthread_t tid2;
    pthread_create(&tid2, 0, &nothing2, NULL);
    float f = 8.0f;
  } else {
    pthread_t tid2;
    pthread_create(&tid2, 0, &nothing2, NULL);
  }
}
