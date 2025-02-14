// PARAM: --set ana.activated[+] memOutOfBounds
#include <pthread.h>

struct c {
  int *conn;
} d();

int main() {
  int str = {0};
  struct c axel = {0};
  axel.conn = &str;
  int *ptr = (axel.conn + rand ());
  int x = *ptr + 1; // WARN
}