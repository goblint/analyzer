typedef int myint;

typedef struct {
  int f;
} s;

int main() {
  myint x = 42;
  void *p = &x;

  s a;
  a.f = 43;
  void *q = &a;
  return 0;
}
