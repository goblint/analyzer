// PARAM: --set ana.activated[+] var_eq
// ldv-benchmarks: u__linux-concurrency_safety__drivers---net---ethernet---ethoc.ko.cil.c
#include <goblint.h>

struct resource {
   char const *name ;
   unsigned long flags ;
   struct resource *parent ;
   struct resource *sibling ;
   struct resource *child ;
};

struct resource *magic();

int main() {
  struct resource *res = (struct resource *)0;
  res = magic();

  if (res == (struct resource *)0)
    __goblint_check(1); // reachable
  else
    __goblint_check(1); // reachable

  return 0;
}