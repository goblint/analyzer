// memcpy, memmove, memset, strcpy, strncpy, strcat and strncat return their destination argument.
// mempcpy and memccpy return another pointer, so the analysis only forgets the lvalue's previous value.
#define _GNU_SOURCE
#include <goblint.h>
#include <string.h>

int main() {
  char old[8] = "abc";
  char src[8] = "xyz";
  char other[8];
  void *p; // same type as the mem* functions return, so CIL assigns the call to p directly, without a temporary
  char *s; // likewise for the str* functions

  p = other;
  p = memcpy(old, src, 4);
  __goblint_check(p == old);

  char *c = memcpy(old, src, 4); // char * differs from memcpy's void *, so CIL assigns the call to a temporary first
  __goblint_check(c == old);

  p = old;
  p = memcpy(p, src, 4); // the destination is read from the lvalue itself
  __goblint_check(p == old);

  p = other;
  p = memmove(old, src, 4);
  __goblint_check(p == old);

  p = other;
  p = memset(old, 0, 4);
  __goblint_check(p == old);

  s = other;
  s = strcpy(old, src);
  __goblint_check(s == old);

  s = other;
  s = strncpy(old, src, 4);
  __goblint_check(s == old);

  s = other;
  s = strcat(old, src);
  __goblint_check(s == old);

  s = other;
  s = strncat(old, src, 1);
  __goblint_check(s == old);

  p = other;
  p = mempcpy(old, src, 4);
  __goblint_check(p == other); // UNKNOWN!

  p = other;
  p = memccpy(old, src, 'y', 4);
  __goblint_check(p == other); // UNKNOWN!

  return 0;
}
