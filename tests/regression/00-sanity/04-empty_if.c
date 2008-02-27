#include <assert.h>

int main()
{
  int i;
  if (i);
  ++ i;
  assert(i); // UNKNOWN!
  return 0;
}
