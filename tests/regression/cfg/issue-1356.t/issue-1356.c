extern int  __VERIFIER_nondet_int(void);

extern void abort(void);
void assume_abort_if_not(int cond) {
  if(!cond) {abort();}
}

int minus(int a, int b) {
  assume_abort_if_not(b <= 0 || a >= b - 2147483648); // there shouldn't be invariants 1 <= b and b != 0 before this line
  assume_abort_if_not(b >= 0 || a <= b + 2147483647); // there shouldn't be invariants b <= -1 and b != 0 before this line
  return a - b;
}

int main() {
  int x, y;
  x = __VERIFIER_nondet_int();
  y = __VERIFIER_nondet_int();
  minus(x, y);
  return 0;
}
