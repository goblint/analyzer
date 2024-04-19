// Should not crash (issue 1421)
#include<pthread.h>
struct a {
  pthread_mutex_t b;
};
struct c {
  struct a *conn;
};

int main() {
  int x;
  struct a str = {0};
  struct c axel = {0};
  axel.conn = &str;
  pthread_mutex_t* ptr = &((axel.conn + 0)->b);
  x = 4;
  pthread_mutex_lock(ptr);
  pthread_mutex_unlock(ptr);
  pthread_mutex_lock(&((axel.conn + 0)->b));
}
