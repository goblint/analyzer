//PARAM: --enable sem.noreturn.dead_code
// test that (calls of) functions marked noreturn are detected as dead
#include <stdnoreturn.h>
#include <stdio.h>


noreturn void does_not_return() {
  abort();
}

noreturn void does_return() {

} // WARN


// with nested calls to noreturn functions, only warn that the innermost function returns

noreturn void chain0() {

} // WARN

noreturn void chain1() {
  chain0();
} // NOWARN!

noreturn void chain2() {
  chain1();
} // NOWARN!


int main() {
  // switch over an unknown int, otherwise the first call
  // to a function that aborts will dead-code the rest of main
  int unknown;
  scanf("%d", &unknown);
  switch (unknown) {
    case 0:
      does_not_return();
      __goblint_check(0); // NOWARN (unreachable)
      break;

    case 1:
      does_return();
      __goblint_check(0); // NOWARN (unreachable)
      break;

    case 2:
      chain2();
      break;
  }
}
