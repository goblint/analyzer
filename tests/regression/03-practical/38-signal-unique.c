#include <signal.h>

int g = 0;

void handler(int sig) {
  g = 1; // NORACE
  g = 2; // NORACE
}

int main() {
  signal(SIGTERM, handler);
  return 0;
}
