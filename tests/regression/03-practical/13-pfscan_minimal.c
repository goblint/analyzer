#include <goblint.h>

extern int get(void);

int init(void) { return 0; }

int main(int argc , char **argv )
{
  int tmp = 5;
  init();
  tmp = get();
  __goblint_check(tmp == 5); // UNKNOWN
  return 0;
}
