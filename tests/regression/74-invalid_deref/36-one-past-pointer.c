// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>
#include <stdio.h>

int main(void) {
  char *buf = malloc(4);
  char *end;

  end = buf + 3; // NOWARN
  printf("%s", end); // NOWARN
  printf("%p", (void *) end); // NOWARN
  printf("%s", end + 1); // TODO WARN! (unsound)
  printf("%p", (void *) (end + 1)); // TODO WARN (imprecise due to %p)

  end = buf + 4; // NOWARN
  printf("%s", end); // WARN!
  printf("%p", (void *) end); // WARN (imprecise due to %p)
  printf("%s", end + 1); // WARN! (unsound)
  printf("%p", (void *) (end + 1)); // WARN (imprecise due to %p)
  printf("%s", end - 1); // TODO NOWARN (imprecise)
  printf("%p", (void *) (end - 1)); // TODO NOWARN (imprecise)

  end = buf + 5; // NOWARN
  printf("%s", end); // WARN!
  printf("%p", (void *) end); // WARN (imprecise due to %p)
  printf("%s", end + 1); // WARN!
  printf("%p", (void *) (end + 1)); // WARN (imprecise due to %p)
  printf("%s", end - 1); // WARN!
  printf("%p", (void *) (end - 1)); // WARN (imprecise due to %p)

  free(buf);
  return 0;
}
