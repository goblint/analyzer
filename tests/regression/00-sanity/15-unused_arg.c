int glob = 0;

void f() {
  glob++; // NOWARN!
}

void (*fptr)(void) = f;

int g(void (*ptr)(void)) {
  return 0;
}

int main() {
  g(fptr);
  return glob;
}
