// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState
#include <goblint.h>
#include <stdlib.h>

struct Pair {
  int (*first)[7];
  int second;
};

struct Crazy {
  int whatever;
  int arr[5];
};

void main(void) {
  int arr[7] = {1, 2, 3, 4, 5, 6, 7};
  int(*x)[7] = (int(*)[7])malloc(sizeof(int));
  struct Pair p;
  p.first = x;
  p.second = (*x)[3];

  struct Pair p2;
  p2.first = x;

  __goblint_check(p.first == p2.first);

  int arr2[2][2] = {{1, 2}, {1, 2}};
  p.second = arr2[1][1];

  // int *test;

  // int *x2[2] = {test, test};

  // int test2 = *(x2[1]);

  struct Crazy crazyy[3][2];

  __goblint_check(crazyy[2][1].arr[4] == ((struct Crazy *)crazyy)[5].arr[4]);

  int *sx[4];
  int k = *sx[1];
}
