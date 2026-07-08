//PARAM: --set ana.activated[+] apron  --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit
// NOCRASH
#include <pthread.h>

void helper(void) {
  return;
}

int main(void) {
  pthread_t t;
  int x;
  helper();
  x = 8;


  return 0;
}
