// PARAM: --enable ana.int.interval --enable ana.int.def_exc
// from: ldv-linux-3.0/usb_urb-drivers-vhost-vhost_net.ko.cil.out.i
typedef unsigned long long u64;

int main( )
{ u64 a ;
  unsigned long flag ;
  unsigned long roksum ;
  struct thread_info *tmp___7 ;
  int tmp___8 ;
  long tmp___9;
  void *log_base;
  unsigned long sz;
  u64 addr;
  {
  a = (addr / 4096ULL) / 8ULL;
  if (a > (u64 )(0x0fffffffffffffffUL - (unsigned long )log_base)) {
    return (0);
  } else {
    if (a + (u64 )((unsigned long )log_base) > 0x0fffffffffffffffULL) {
      return (0);
    } else {
    }
  }
  {
  tmp___7 = current_thread_info();
  // __asm__ ("add %3,%1 ; sbb %0,%0 ; cmp %1,%4 ; sbb $0,%0": "=&r" (flag), "=r" (roksum): "1" (log_base + a),
  //           "g" ((long )((((sz + 32768UL) - 1UL) / 4096UL) / 8UL)), "rm" (tmp___7->addr_limit.seg));
  }
  if (flag == 0UL) {
    tmp___8 = 1;
  } else {
    tmp___8 = 0;
  }
  {
  tmp___9 = __builtin_expect((long )tmp___8, 1L);
  }
  return ((int )tmp___9);
  }
}
