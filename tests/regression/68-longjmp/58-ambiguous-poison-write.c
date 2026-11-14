// PARAM: --set ana.activated[+] poisonVariables --set ana.activated[+] modifiedSinceSetjmp
#include <setjmp.h>
#include <stdlib.h>
jmp_buf env;
int main(void) {
  int a=0, b=0;
  if (setjmp(env) == 0) { a=1; b=1; longjmp(env, 1); }
  int *p = rand() ? &a : &b;
  *p = 0;
  return a + b; // WARN
}
