#include <string.h>
#include <assert.h>

int main()
{
  int* x = malloc(sizeof(int));
  int* y = malloc(sizeof(int));

  *x = 0;
  *y = 1;

  assert(*x == 0);
  assert(*y == 1);

  return 0;
}
