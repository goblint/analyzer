// PARAM: --set kernel true
#include <linux/device.h>

int __init start (unsigned count) {
  if (count)
    return -1;
  assert(0); // FAIL
  return 0;
}
