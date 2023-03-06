// PARAM: --set kernel true
#include <goblint.h>
#include <linux/device.h>

int __init start (unsigned count) {
  if (count)
    return -1;
  __goblint_check(0); // FAIL
  return 0;
}
