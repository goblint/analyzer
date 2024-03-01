#include <goblint.h>
int main ()
{
  int x;
  int three = 3;

  int y = (three == &x);

  // &x equaling the constant 3 is exceedinly unlikely
  __goblint_check(y == 1); //UNKNOWN!

  return 0;
}
