#include <string.h>

int f() {
  int x;
  return x;
}

void copy1(void* dst, const void* src);
void copy2(const void* dst, const void* src);
void* copy3(const void* src);
void copy4(void** dst, void* src) {
  *dst = src;
}

int main(){
  int (*g)();
  // g = &f;
  // f == &f
  // g != &g
  // g = &f; // ok, reachable, g -> {f}
  // copy1(g, &f); // reachable, but g -> {null, safe}
  // copy2(g, &f); // reachable, but shouldn't, g -> {null, safe}
  // g = copy3(&f); // reachable, but g -> {null, safe, ?}
  memcpy(&g, &f, sizeof g); // unreachable!, g -> {null, safe}
  // copy4((void**)&g, &f); // ok, reachable, g -> {f}
  return g();
}
