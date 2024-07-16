#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;

int main () {
  if (setjmp(env_buffer)) {
    __goblint_check(1); // reachable
    return 8;
  }

  longjmp(env_buffer, 0); // WARN
  __goblint_check(0); // NOWARN
  return 0;
}
