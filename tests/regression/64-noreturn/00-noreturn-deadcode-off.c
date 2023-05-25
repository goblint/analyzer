//PARAM: --disable sem.noreturn.dead_code
// tests for warnings about functions marked noreturn, that may return during program execution
#include <stdnoreturn.h>
#include <stdio.h>


noreturn void empty() {

} // WARN

noreturn void aborts() {
  abort();
} // NOWARN!


noreturn void foo(int x) {
  if (x < 0) {
    return 7; // WARN
  }
  abort();
}

noreturn void bar(int x) {
  if (x < 0) {
    return 7; // NOWARN!
  }
  abort();
}


int main() {
  // switch over an unknown int, otherwise the first call
  // to a function that aborts will dead-code the rest of main
  int unknown;
  scanf("%d", &unknown);
  switch (unknown) {
    case 0:
      empty();
      break;

    case 1:
      aborts();
      break;

    case 3:
      foo(-1);
      break;

    case 4:
      foo(1);
      break;

    case 5:
      bar(1);
      break;
  }
}
