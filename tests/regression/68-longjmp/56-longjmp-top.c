// Extracted from concrat/pigz.
#include <setjmp.h>
#include <pthread.h>
#include <goblint.h>

pthread_key_t buf_key;

int main() {
  jmp_buf buf;
  pthread_setspecific(buf_key, &buf);

  if (!setjmp(buf)) {
    jmp_buf *buf_ptr;
    buf_ptr = pthread_getspecific(buf_key);
    longjmp(*buf_ptr, 1); // NO CRASH: problem?!
  }
  else {
    __goblint_check(1); // TODO reachable: https://github.com/goblint/analyzer/pull/1210#discussion_r1350021903
  }
  return 0;
}
