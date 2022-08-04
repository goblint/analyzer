//PARAM: --enable ana.int.interval
#include <assert.h>

extern int printf(char *, ...);

main() {
  int sum = 0;
  int i = 0;
  while (i < 11) {
    sum += i;
    i++;
  }
  __goblint_check(i == 11);
  printf("%d\n",sum);
}
