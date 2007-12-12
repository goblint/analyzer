#include <assert.h>

int get(void);

int init(void) { return 0; }

int main(int argc , char **argv ) 
{
  int tmp;
  init();
  tmp = get();
  assert_unknown(tmp);
  return 0;
}
