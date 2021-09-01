// SKIP PARAM: --sets ana.activated[+] apron
#include <assert.h>

void main() {
  int x;
  {
//   if (1 <= x) {
//   if (-10 <= x) {
    if (x <= 10) {
      assert((x / 3) <= 4);
      assert((x / 3) <= 3); // TODO: why does apron think the upper bound is 13/3 if no (positive) lower bound is known?
    }
  }
}
