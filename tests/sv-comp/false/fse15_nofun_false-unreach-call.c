extern void __VERIFIER_error() __attribute__ ((__noreturn__));

int main() {
  int s; // = __VERIFIER_nondet_int()
  int t; // = __VERIFIER_nondet_int()

  int d = s - t;
  // assumption: d >= 2 && d <= 8
  // control: condition-false
  if (d < 2 || d > 8)
    return 0;
  int x; // = __VERIFIER_nondet_int()
  // assumption: x != 0 or x == 1
  // int a = x ? 512 : 64;
  int a = x ? 64 : 512;
  // assumption: a == 512 && d >= 4 (or d == 4)
  int b = a * d;
  // control: condition-true
  if (b >= 2048)
    __VERIFIER_error();
  // sink
  if (b < 128)
    __VERIFIER_error();
  while (a > 0)
    a--;

  return 0;
}
