#include <assert.h>

int main() {
  int arr[10];
  int r; // rand
  int *p;
  if (r) {
    p = &arr[1];
    assert(1); // extra node in cfg
    // p -> {arr[1]}
  }
  else {
    p = &arr[2];
    assert(1); // extra node in cfg
    // p -> {arr[2]}
  }
  // OLD undesired behavior: p -> {arr[top]}
  // NEW desired behavior: p -> {arr[1], arr[2]}

  // OLD explanation:
  // even though we use HoarePO for addresses per-varinfo
  // and lattice operations are on offsets
  // neither p is leq of another, so Hoare ordering should keep both
  // but address domain actually joins them nevertheless

  // NEW explanation:
  // using actual Hoare set instead of HoarePO
  // correctly keeps both as maximal elements

  // automatic check of new behavior
  assert(p == &arr[1] || p == &arr[2]);
  return 0;
}
