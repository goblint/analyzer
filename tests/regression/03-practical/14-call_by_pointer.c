// PARAM: --set ana.activated "['base','escape','mallocWrapper']"
#include <assert.h>

/**
 * foo /might/ call the argument function
 */
extern void foo(void (*)(void));

int glob;

void reset_glob(void)
{
  int n;
  glob = n;
}

int main()
{
  glob = 0;
  foo(reset_glob);
  assert(glob == 0); // UNKNOWN
  return 0;
}
