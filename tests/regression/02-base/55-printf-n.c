// SKIP
#include <stdio.h>
#include <assert.h>

int main() {
  int written = 0;
  printf("foo%n\n", &written); // invalidates written by setting written = 3
  __goblint_check(written != 0); // TODO (fail means written == 0, which is unsound)

  printf("%d\n", written);
  return 0;
}
