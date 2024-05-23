#include <pthread.h>

struct a {
  pthread_mutex_t b;
};
struct c {
  struct a *conn;
} d();

int main() {
  struct a str = {0};
  struct c axel = {0};
  axel.conn = &str.b;
  pthread_mutex_t* ptr = axel.conn + rand();
  pthread_mutex_lock(ptr);
  pthread_mutex_unlock(ptr); // WARN
}