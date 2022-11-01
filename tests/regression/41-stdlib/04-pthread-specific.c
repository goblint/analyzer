#include<pthread.h>
#include<goblint.h>

pthread_key_t key;

void *t_fun(void *arg) {
  int var = 8;
  pthread_setspecific(key,&var);
  int* ptr = (int*)pthread_getspecific(key);
  *ptr = 12;
  __goblint_check(var == 8); //UNKNOWN!
}

int main (void)
{
    pthread_t tid1, tid2;

    pthread_key_create (&key, NULL);
    pthread_create (&tid1, NULL, (void *)t_fun, NULL);
    pthread_create (&tid2, NULL, (void *)t_fun, NULL);
    pthread_join (tid1, NULL);
    pthread_join (tid2, NULL);

    pthread_key_delete(key);

    return 0;
}
