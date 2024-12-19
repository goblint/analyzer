// SKIP PARAM: --set ana.activated[+] apron --set sem.int.signed_overflow assume_none
#include <pthread.h>
#include <goblint.h>
#include <stdio.h>

int debug;
int other;

int main() {
  int top;

  // Needed so Base & DefExc doesn't find this information because it invalidates less
  if(top) {
    debug = 3;
  }

  fscanf(stdin, "%d", &other);

   // Fails as debug is invalidated
  __goblint_check(debug <= 3);
  return 0;
}
