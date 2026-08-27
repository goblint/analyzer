#include <signal.h>
#include <goblint.h>

void handler(int sig) {
  __goblint_check(sig == SIGTERM);
}

int main() {
  signal(SIGTERM, handler);
  return 0;
}
