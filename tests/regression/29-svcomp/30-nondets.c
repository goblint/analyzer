// PARAM: --enable ana.sv-comp.enabled --enable ana.sv-comp.functions --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )"
#include <pthread.h>
#include <goblint.h>

// Global is never accessed, hence information about protecting mutexes is implicit \bot
int g1 = 5;

int main(void) {
  #ifdef __APPLE__
    // OS X headers have different names for some types
  #else
    size_t x1 = __VERIFIER_nondet_size_t();
    __U16_TYPE x2 = __VERIFIER_nondet_u16();
    __uint8_t x3 =__VERIFIER_nondet_u8();
    __U32_TYPE x4 = __VERIFIER_nondet_u32();
    size_t x5 = __VERIFIER_nondet_loff_t();
    __uint128_t x6 = __VERIFIER_nondet_uint128();
    __int128_t x6andahalf = __VERIFIER_nondet_int128();
    long long x7 = __VERIFIER_nondet_longlong();
    unsigned long long x8 = __VERIFIER_nondet_ulonglong();
    unsigned char x9 = __VERIFIER_nondet_unsigned_char();
    const char* ptr1 = __VERIFIER_nondet_const_char_pointer();
    char* ptr2 = __VERIFIER_nondet_charp();
  #endif

  __goblint_check(g1 == 5); // Should not invalidate
  return 0;
}
