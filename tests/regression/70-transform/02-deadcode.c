// SKIP: this is an input file for cram tests

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
int basic2(int n) {
  int a = 0;

  if (n < 0)
    return 0;

  for (int i = 0; i < n; i++)
    a += i + n;  // bug in dead code detection: is not found dead by goblint, because it is not included in the result

  return a;
}


int one_branch_dead(int x) {
  if (x > 7)
    return x;
  else
    return 7 - x;
}



int uncalled_but_referenced_function(int x) {
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

// called with u > 12
int loop_continue_break(int u) {
  int w = 12;
  for (int t = 0; t < u; t++) {
    w += t;
    if (t > 3) continue;
    w += u;
    if (t > 7) break;
    w += w;
  }
  return w;
}

int loop_dead_on_break(int z) {
  int s = 5;
  for (int i = 0; i < z; i++) {
    s += i;
    if (i < 5) break;
    s += s;  // dead
  }
  return s;
}

int compound_statement_in_out() {
  goto in1;

  // condition is dead, must not remove if statement's body though
  if (1) {
    in1:
    goto in2;
  }

  while (1) {
    in2:
    goto in3;
  }

  for (int i = 0; i < 10; i++) {
    in3:
    goto out;
  }

  out:
  return 0;
}


int main() {
  // test calls in multiple contexts
  basic1(7);
  basic1(3);
  basic1(6);
  basic2(-3);
  basic2(-6);
  basic2(-12);

  one_branch_dead(9);
  one_branch_dead(12);
  int (*f)(int) = &uncalled_but_referenced_function;

  conditional_call_in_loop(5);
  loop_continue_break(11);
  loop_dead_on_break(3);

  compound_statement_in_out();

  abort();

  // calls from dead code don't count
  uncalled1();
  uncalled_but_referenced_function(3);
}
