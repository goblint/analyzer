// see https://github.com/goblint/cil/issues/41
// If goblint pre-processes a file including stdlib.h on an Apple M1 machine,
// it will include /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/mach/arm/_structs.h
// which uses __uint128_t which is somehow not defined before.

#if defined(__arm64__)
  // typedef unsigned __int128 uint128_t; // https://stackoverflow.com/a/34588884 TODO __int128 exists in CIL but fails; see https://github.com/goblint/cil/issues/41
  typedef unsigned long long __uint128_t; // TODO wrong! use the above once fixed. This the definition for __uint64_t
#endif
