#include <goblint.h>

int main()
{
  int i;
  if (i);
  ++ i;
  __goblint_check(i); // UNKNOWN!
  return 0;
}
