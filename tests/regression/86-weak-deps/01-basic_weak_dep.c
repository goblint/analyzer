// PARAM: --set "ana.activated[+]" signs
// Fully context-insensitive
#include <goblint.h>
#include <pthread.h>

int g = 0;
int y = 0;

void *t_foo(void *arg) {
  if (g == 1) {
     y = 1;
  }

}

int main() {
  int x;
  int k = 1;
  int l = 2;
  int m = 3;
  int n = 4;
  
  pthread_t id;
  pthread_create(&id, NULL, t_foo, NULL);
  int a = 1; // should not be re-evaluated 
  int b = 2; // should not be re-evaluated 
  int c = 3; // should not be re-evaluated 
  int d = 4; // should not be re-evaluated 
  g = 1;
  x = y;
  int d = 4;

  __goblint_check(y < 2); // SUCCESS
  __goblint_check(y == 1); // UNKNOWN!
  __goblint_check(x < 2); // SUCCESS
  __goblint_check(x == 1); // UNKNOWN!
  return 0;
}
