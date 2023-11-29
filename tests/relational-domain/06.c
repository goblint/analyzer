#include <stdio.h>
#include <goblint.h>

int main() {
  int arr[2] = {10, 20};
  int x = arr[0];
  int y = arr[1];

  arr[0] = 20;

  __goblint_check(x != y);

  return 0;
}