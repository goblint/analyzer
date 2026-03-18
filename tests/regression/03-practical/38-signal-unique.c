#include <signal.h>

int g = 0;

void handler(int sig) {
  g = 1; // TODO NORACE
  g = 2; // TODO NORACE
}

int main() {
  signal(SIGTERM, handler);
  return 0;
}
