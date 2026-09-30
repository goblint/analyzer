// printf's %n conversion writes the number of characters output so far
// through its argument, but every printf-family spec (printf, fprintf,
// sprintf, snprintf, dprintf, syslog, the wide-character and _chk variants,
// error, warn(x), err(x)) used to mark its variadic arguments read-only:
// Base never invalidated n below, so __goblint_check(n == 42) proved true
// although the call had already overwritten n.
//
// format_excludes_percent_n in libraryFunctions.ml now reads the format
// argument when it folds to a string literal and parses each '%' directive
// per the POSIX fprintf grammar -- an optional positional argument, flags,
// width, precision, and length modifier, before the conversion character --
// so a call whose format has no n conversion keeps its varargs read-only,
// and one that does still has them invalidated. A first version of this
// check only searched the format for the literal substring "%n", which
// missed every n conversion carrying a length modifier (%hn, %hhn, %ln,
// %lln, %jn, %zn, %tn, %Ln) or a POSIX positional argument (%1$n), and so
// wrongly kept their arguments read-only; this file's cases below cover
// each of them. A second version accepted any character after the length
// modifier as a non-writing conversion, so C23's wN and wfN modifiers
// (%w32n, %wf32n) were read as a 'w' conversion followed by text; a
// directive now counts as non-writing only when it ends in a known reading
// conversion. A non-literal format still gets the previous, uniform [r; w]
// treatment: the caller cannot read it either.

#include <goblint.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

int main(void) {
  int n = 42;
  printf("abc%n", &n);
  __goblint_check(n == 42); // UNKNOWN (printf's %n has already written through n)

  short hn = 42;
  printf("abc%hn", &hn);
  __goblint_check(hn == 42); // UNKNOWN (the h length modifier before n)

  signed char hhn = 42;
  printf("abc%hhn", &hhn);
  __goblint_check(hhn == 42); // UNKNOWN (the hh length modifier before n)

  long ln = 42;
  printf("abc%ln", &ln);
  __goblint_check(ln == 42); // UNKNOWN (the l length modifier before n)

  long long lln = 42;
  printf("abc%lln", &lln);
  __goblint_check(lln == 42); // UNKNOWN (the ll length modifier before n)

  intmax_t jn = 42;
  printf("abc%jn", &jn);
  __goblint_check(jn == 42); // UNKNOWN (the j length modifier before n)

  size_t zn = 42;
  printf("abc%zn", &zn);
  __goblint_check(zn == 42); // UNKNOWN (the z length modifier before n)

  ptrdiff_t tn = 42;
  printf("abc%tn", &tn);
  __goblint_check(tn == 42); // UNKNOWN (the t length modifier before n)

  long Ln = 42;
  printf("abc%Ln", &Ln);
  __goblint_check(Ln == 42); // UNKNOWN (the L length modifier before n)

  int positional = 42;
  printf("abc%1$n", &positional);
  __goblint_check(positional == 42); // UNKNOWN (a POSIX positional n conversion, %1$n)

  int widthn = 42;
  printf("abc%5n", &widthn);
  __goblint_check(widthn == 42); // UNKNOWN (a width before n, %5n)

  int starn = 42;
  printf("abc%*n", 3, &starn);
  __goblint_check(starn == 42); // UNKNOWN (a '*' width before n, %*n)

  int32_t w32n = 42;
  printf("abc%w32n", &w32n);
  __goblint_check(w32n == 42); // UNKNOWN (C23's w32 length modifier before n)

  int_fast32_t wf32n = 42;
  printf("abc%wf32n", &wf32n);
  __goblint_check(wf32n == 42); // UNKNOWN (C23's wf32 length modifier before n)

  int unknown = 42;
  printf("abc%k", &unknown);
  __goblint_check(unknown == 42); // UNKNOWN (k is no conversion the parser knows, so it may write)

  int d = 5;
  printf("%d\n", d);
  __goblint_check(d == 5); // plain %d has no n conversion: stays read-only, proven

  long mixed = 5;
  printf("%ld %s %5.2f %1$ld %%n\n", mixed, "x", 1.0);
  __goblint_check(mixed == 5); // modifiers, a positional argument and %%n, but no n conversion: proven

  return 0;
}
