// PARAM: --enable annotation.int.enabled
#include <stdlib.h>
#include <goblint.h>
struct slotvec {
   size_t size ;
   char *val ;
};
static char slot0[256] ;
static struct slotvec slotvec0 = {sizeof(slot0), slot0};

static void install_signal_handlers(void)
{
  { if(!(slotvec0.val == & slot0[0LL])) {  reach_error(); abort(); } };
}

int main(int argc , char **argv )
{
  // Goblint used to consider both branches in this condition to be dead, because the meet on addresses with different active int domains was broken
  { if(!(slotvec0.val == & slot0[0LL])) { reach_error(); abort(); } };
  install_signal_handlers();

  // Should be reachable
  __goblint_check(1);
  return 0;
}
