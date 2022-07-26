#include <assert.h>

extern int printf();
extern int scanf();
int g;

int proov(int x) {
  int a;
  a = 0;
  switch (x) {
    case 1:
      a++;
      break;
    case 2:
      a += 2;
      break;
    case 3:
      a += 7;
      break;
    default:
      a--;
  }
  g = a;
  return a;
}

int main() {
  int i;
  scanf("%d",&i);
  printf("%d\n",proov(i));
  assert(g == 0); //FAIL
}
