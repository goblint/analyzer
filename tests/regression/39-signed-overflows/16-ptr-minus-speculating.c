// PARAM: --enable ana.int.interval

int main() {
  void *p;
  long long i;
  void *q = p - i; // NOWARN (signed integer overflow in unary -)
  return 0;
}
