// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>
#include <stdio.h>

int main(void) {
  char *buf = malloc(4);
  char *end;
  end = buf + 4; //NOWARN
  printf("%p", (void *) end); //NOWARN
  printf("%c", *end); //WARN
  free(buf);
  return 0;
}
