// see https://github.com/goblint/cil/issues/41
// If goblint pre-processes a file including stdlib.h on an Apple M1 machine,
// it will include /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/mach/arm/_structs.h
// which uses __uint128_t which is somehow not defined before.

#if defined(__arm64__)
  // typedef unsigned __int128 uint128_t; // https://stackoverflow.com/a/34588884 __int128 exists in gcc and in CIL but fails there; see https://github.com/goblint/cil/issues/41
  typedef unsigned long long __uint128_t; // TODO Use the above once fixed! This is wrong since long long is just 64 bit. See https://github.com/goblint/cil/issues/41#issuecomment-893266843
#endif
