// old error function
void __VERIFIER_error() __attribute__((goblint_stub));
void __VERIFIER_error() { abort(); }
// new error function (https://github.com/sosy-lab/sv-benchmarks/pull/1077)
// followed by abort() in benchmarks
// implemented in benchmarks
// void reach_error() { }

// Some files define __VERIFIER_assume, some declare as extern. What happens when redefined?
void __VERIFIER_assume(int expression) __attribute__((goblint_stub));
void __VERIFIER_assume(int expression) { if (!expression) { LOOP: goto LOOP; }; return; }

// #define __VERIFIER_nondet(X) X __VERIFIER_nondet_##X() { X val; return val; }
#define __VERIFIER_nondet2(X, Y) \
    X __VERIFIER_nondet_##Y() __attribute__((goblint_stub)); \
    X __VERIFIER_nondet_##Y() { X val; return val; }
#define __VERIFIER_nondet(X) __VERIFIER_nondet2(X, X)

__VERIFIER_nondet2(_Bool, bool)
__VERIFIER_nondet(char)
// int __VERIFIER_nondet_int() { int val; return val; }
__VERIFIER_nondet(int)
__VERIFIER_nondet(float)
__VERIFIER_nondet(double)
// __VERIFIER_nondet(loff_t)
__VERIFIER_nondet(long)
__VERIFIER_nondet2(char*, pchar)
// missing pthread_t
// missing sector_t
__VERIFIER_nondet(short)
// __VERIFIER_nondet(size_t)
// missing u32
__VERIFIER_nondet2(unsigned char, uchar)
__VERIFIER_nondet2(unsigned int, uint)
__VERIFIER_nondet2(unsigned long, ulong)
__VERIFIER_nondet2(unsigned, unsigned)
__VERIFIER_nondet2(unsigned short, ushort)
// void* __VERIFIER_nondet_pointer() { void* val; return val; }
__VERIFIER_nondet2(void*, pointer)

// atomics are now special in Goblint
// void __VERIFIER_atomic_begin() { }
// void __VERIFIER_atomic_end() { }