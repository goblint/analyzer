extern void abort(void);
void reach_error(){}

void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: {reach_error();abort();}
  }
  return;
}

int main() {
  void *p;
  void *q = &p;
  void *r = &p;
  p = &q;

  return 0;
}

// ((p == & q && *p == & p) && (q == & p && *q == & q)) && (r == & p && (*r == & q && *(*r) == & p))