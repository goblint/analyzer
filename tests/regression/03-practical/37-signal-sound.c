#include <signal.h>
#include <goblint.h>

int g = 0;

void handler(int sig) {
  g = 1;
}

int main() {
  __goblint_check(g == 0);
  signal(SIGTERM, handler);
  __goblint_check(g == 0); // UNKNOWN!
  return 0;
}
