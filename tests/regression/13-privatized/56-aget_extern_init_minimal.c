// SKIP
#include <pthread.h>
#include <stdlib.h>

extern int optind ;

pthread_t hthread  ;

void *signal_waiter(void *arg )
{
}

int main(int argc , char **argv )
{
  pthread_create(& hthread, NULL, & signal_waiter, NULL);

  if (optind >= argc) {
    if (optind == argc) {
      // lock priv should also read Unknown int, not Unknown here
      exit(1);
    }
  }

  return (0);
}