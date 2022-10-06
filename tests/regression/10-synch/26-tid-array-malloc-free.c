// PARAM: --set ana.activated[+] thread
// NOCRASH
#include <pthread.h>
#include <stdlib.h>

void *t_fun(void *arg) {
  return NULL;
}

int main(void) {
  pthread_t *t_ids = malloc(sizeof(pthread_t) * 10000);
  for (int i = 0; i < 10000; i++)
    pthread_create(&t_ids[i], NULL, t_fun, NULL);
  for (int i = 0; i < 10000; i++)
    pthread_join (t_ids[i], NULL);
  free(t_ids);
  return 0;
}