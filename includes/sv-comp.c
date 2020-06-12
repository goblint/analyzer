// old error function
void __VERIFIER_error() { abort(); }
// new error function (https://github.com/sosy-lab/sv-benchmarks/pull/1077)
// followed by abort() in benchmarks
void reach_error() { }

// Some files define __VERIFIER_assume, some declare as extern. What happens when redefined?
void __VERIFIER_assume(int expression) { if (!expression) { LOOP: goto LOOP; }; return; }

// #define __VERIFIER_nondet(X) X __VERIFIER_nondet_##X() { X val; return val; }
#define __VERIFIER_nondet2(X, Y) X __VERIFIER_nondet_##Y() { X val; return val; }
#define __VERIFIER_nondet(X) __VERIFIER_nondet2(X, X)

__VERIFIER_nondet2(_Bool, bool)
__VERIFIER_nondet(char)
__VERIFIER_nondet2(unsigned char, uchar)
// int __VERIFIER_nondet_int() { int val; return val; }
__VERIFIER_nondet(int)
__VERIFIER_nondet2(unsigned int, uint)
__VERIFIER_nondet(long)
__VERIFIER_nondet2(unsigned long, ulong)
// void* __VERIFIER_nondet_pointer() { void* val; return val; }
__VERIFIER_nondet2(void*, pointer)

void __VERIFIER_atomic_begin() { } // TODO: use atomic marker in Goblint
void __VERIFIER_atomic_end() { } // TODO: use atomic marker in Goblint