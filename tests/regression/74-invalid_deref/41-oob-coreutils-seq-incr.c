// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>
#include <string.h>

// This is some wild C code from coreutils seq.
static void
incr (char **s0, size_t *s_len)
{
  char *s = *s0; // NOWARN
  char *endp = s + *s_len - 1; // NOWARN
  do
    {
      if ((*endp)++ < '9') // TODO NOWARN
        return;
      *endp-- = '0'; // TODO NOWARN
    }
  while (endp >= s);
  *--(*s0) = '1'; // TODO WARN!
  ++*s_len; // NOWARN
}

int main() {
  char* s = malloc(10);
  memset(s, '0', 10); // NOWARN

  char* s2 = s + 9;
  size_t len = 1;
  incr(&s2, &len); // everything in this should be in bounds

  memset(s, '9', 10); // NOWARN

  s2 = s;
  len = 10;
  incr(&s2, &len); // something in this could be out of bounds

  return 0;
}
