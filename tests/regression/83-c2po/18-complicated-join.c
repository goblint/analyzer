// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts
// TODO add configuration to use the join with automata
// Example 1 from the paper Join Algorithms for the Theory of Uninterpreted
// Functions by Gulwani et al.

#include <goblint.h>
#include <stdlib.h>

void main(void) {
  long ********y = (long ********)malloc(100 * sizeof(long *));
  *y = (long *******)malloc(100 * sizeof(long *));
  **y = (long ******)malloc(100 * sizeof(long *));
  int top;

  if (top) {
    **y = (long ******)y;
    __goblint_check(**y == (long ******)y);
    __goblint_check(******y == (long**)y);
  } else {
    ***y = (long ***)y;
    __goblint_check(***y == (long *****)y);
    __goblint_check(******y == (long**)y);
  }

  __goblint_check(******y == (long**)y);
}
