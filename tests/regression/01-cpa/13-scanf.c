#include<stdio.h>
#include <goblint.h>

struct kala {
  int x;
  int y;
};

int main () {
  int i = 4;
  scanf("%d", &i);
  __goblint_check(i == 4); // UNKNOWN

  struct kala k;
  int x;
  int *ip;
  k.x = 4;
  k.y = 7;
  scanf("%d\n",&k.x);
  __goblint_check(k.x == 4); // UNKNOWN
  __goblint_check(k.y == 7);

  k.x = 4;
  k.y = 7;
  if (x)
    ip = &k.x;
  else
    ip = &k.y;
  scanf("%d\n",ip);
  __goblint_check(k.x == 4); // UNKNOWN
  __goblint_check(k.y == 7); // UNKNOWN

  k.x = 4;
  k.y = 7;
  scanf("%d%d\n", &k.x, &k.y);
  __goblint_check(k.x == 4); // UNKNOWN
  __goblint_check(k.y == 7); // UNKNOWN
  return 0;
}
