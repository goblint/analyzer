#include <stdnoreturn.h>

// requires sem.noreturn.dead_code
extern noreturn void abort();


// only called with positive n
int basic1(int n) {
  int a = 0, b = 1, c;

  if (n < 0)
    return 0;

  for (int i = 0; i < n; i++) {
    c = a + b;
    a = b;
    b = c;
  }

  return a;
}

// only called with negative n
// TODO: everything after if statement should be removed
int basic2(int n) {
  int a;

  if (n < 0)
    return 0;

  for (int i = 0; i < n; i++)
    a += i + n;

  return a;
}


int one_branch_dead(int x) {
  if (x > 7)
    return x;
  else
    return 7 - x;
}


// TODO: the body of this function should get removed
int uncalled_function(int x) {
  int y = x + 1;
  return x * y;
}


int uncalled1() {
  return 1;
}

int conditional_call_in_loop(int x) {
  for (int i = 0; i < x; i++) {
    // don't call this function with x > 7, then this will get removed
    if (i > 7) {
      uncalled1();
    }
  }
  return 0;
}


int compound_statement_in_out() {
  goto in;

  // condition is dead, must not remove if statement though
  if (1) {
    in:
    goto out;
  }

  out:
  return;
}


int main() {
  basic1(7);
  basic1(3);
  basic2(-3);
  basic2(-6);
  one_branch_dead(9);
  int (*f)(int) = &uncalled_function;
  conditional_call_in_loop(5);
  compound_statement_in_out();

  abort();

  uncalled_function(3);
}