#include <assert.h>

int main() {
  int arr[10];
  int r; // rand
  int *p;
  if (r) {
    p = &arr[1];
    assert(1); // extra node in cfg
  }
  else {
    p = &arr[2];
    assert(1); // extra node in cfg
  }
  // TODO: even though we use Hoare for addresses per-varinfo
  // and lattice operations are on offsets
  // neither p is leq of another, so Hoare should keep both
  // but address domain actually joins them nevertheless
  assert(p == &arr[1] || p == &arr[2]);
  return 0;
}
