#include <goblint.h>

extern int printf (char *);
extern int scanf (char *, int *);
extern void exit (int);

main () {
  int x;
  scanf("%d",&x);
  if (x == 0) {
    printf("Immediate exit.\n");
    exit(0);
  }
  __goblint_check(x !=0);
  printf("The number was not zero.\n");
  return 0;
}
