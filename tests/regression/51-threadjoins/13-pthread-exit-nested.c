// PARAM: --set ana.activated[+] threadJoins --set ana.path_sens[+] threadflag --set ana.base.privatization mutex-meet-tid
#include <pthread.h>
#include <goblint.h>

static int value;

static void finish(void) {
  value = 9;
  pthread_exit(NULL);
}

static void *worker(void *arg) {
  if (arg)
    finish();
  value = 2;
  return NULL;
}

int main(int argc, char **argv) {
  pthread_t thread;
  pthread_create(&thread, NULL, worker, argc > 1 ? &value : NULL);
  pthread_join(thread, NULL);
  __goblint_check(value == 2); //UNKNOWN!
  __goblint_check(value == 9); //UNKNOWN!
  return 0;
}
