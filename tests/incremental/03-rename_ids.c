int a;
void b();
void c();
main() { b(); c(); }
void d();
void c() {
  switch (a) {
  case 2:
    d();
  }
  b();
}
void d() {}
