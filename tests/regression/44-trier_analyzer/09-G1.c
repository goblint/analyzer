#include <assert.h>

extern int printf(char *, ...);

int i;

int proc() {
  int i;
  for (i = 0; i < 1000000000; i++);
  return 0;
}

main () {
  i = proc();
  __goblint_check(i == 0);
  printf("%d\n", i);
}
