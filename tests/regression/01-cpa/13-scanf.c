#include<stdio.h>
#include<assert.h>

struct kala {
  int x;
  int y;
};

int main () {
  int i = 4;
  scanf("%d", &i);
  assert(i == 4); // UNKNOWN

  struct kala k;
  int x;
  int *ip;
  k.x = 4;
  k.y = 7;
  scanf("%d\n",&k.x);
  assert(k.x == 4); // UNKNOWN
  assert(k.y == 7);

  k.x = 4;
  k.y = 7;
  if (x) 
    ip = &k.x;
  else 
    ip = &k.y;
  scanf("%d\n",ip);
  assert(k.x == 4); // UNKNOWN
  assert(k.y == 7); // UNKNOWN

  k.x = 4;
  k.y = 7;
  scanf("%d%d\n", &k.x, &k.y);
  assert(k.x == 4); // UNKNOWN
  assert(k.y == 7); // UNKNOWN
  return 0;
}
