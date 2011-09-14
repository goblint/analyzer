// SKIP
int glob = 0;

void f() {
  glob++; // NOWARN!
}

void (*fptr) = f;

int g(void (*ptr)()) {
  return 0;
}

int main() {
  g(fptr);
  return glob;
}
