#include <goblint.h>

int main() {
  int i;

  // while loop
  i = 0;
  while (i < 10) {
    i++;
  }

  // for loop
  for (i = 0; i < 10; i++) {
    __goblint_check(1);
  }

  // for loop with empty body
  for (i = 0; i < 10; i++) {

  }

  // for loop with empty increment
  for (i = 0; i < 10;) {
    i++;
  }

  // for loop with empty initializer
  i = 0;
  for (; i < 10; i++) {
    __goblint_check(1);
  }

  // for loop with two initializers
  for (int j = (i = 0); i < 10; i++) {
    __goblint_check(1);
  }

  // do-while loop
  i = 0;
  do {
    i++;
  } while (i < 10);

  return 0;
}
