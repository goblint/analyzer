// SKIP
struct {
  unsigned a;
} b;
int c;
void d();
void f();
void h();
int i();
void main() {
  int e;
  if (e)
    b.a++;
  h();
}
void j() {
  int g = 0;
  i(&g);
}
void h() {
  if (b.a)
    d(j);
}
void k() {}
int i(int l) {
  int m;
  while (m) {
    f(l);
    if (c)
      return (-1);
    d(l);
  }
}
void f() {
  d(b);
  k();
}