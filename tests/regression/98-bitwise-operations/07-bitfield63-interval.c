// PARAM: --enable ana.int.interval
#include <stdint.h>

// NOCRASH: global initializer for 63-bit bitfield used to crash with Z.Overflow in interval domain
// From: sv-benchmarks/c/intel-tdx-module/tdg_vp_vmcall__requirement__invalid_input_bitmap_havoc_memory.i
struct
{
  uint64_t notify_ept_faults : 1;
  uint64_t reserved_63_1 : 63;
} g;

int main() {
  return 0;
}
