#include <pthread.h>
//NOCRASH
struct a {
  pthread_mutex_t b;
};
struct c {
  struct a *conn;
} d();

int main() {
  struct a str = {0};
  struct c axel = {0};
  axel.conn = &str;
  pthread_mutex_t* ptr = &((axel.conn + 0)->b);
  pthread_mutex_lock(ptr);
  pthread_mutex_unlock(ptr);
}