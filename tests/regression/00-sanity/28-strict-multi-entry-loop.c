// PARAM: --enable dbg.cfg.loop-clusters
#include <assert.h>

int cycle2_1() {
  int r = 1; // variable to prevent CIL simplification
  if (r)
    goto cycle2_1_entry1;
  else
    goto cycle2_1_entry2;

cycle2_1_entry1:
  __goblint_check(1); // reachable
cycle2_1_entry2:
  __goblint_check(1); // reachable
  goto cycle2_1_entry1;

  return 0;
}

// separate copy instead of r argument for better HTML viewing of dead code
int cycle2_0() {
  int r = 0; // variable to prevent CIL simplification
  if (r)
    goto cycle2_0_entry1;
  else
    goto cycle2_0_entry2;

cycle2_0_entry1:
  __goblint_check(1); // reachable
cycle2_0_entry2:
  __goblint_check(1); // reachable
  goto cycle2_0_entry1;

  return 0;
}

// https://github.com/goblint/analyzer/issues/382#issuecomment-946966693
void loops_s3_min() {
  while (1) {
    if (1) {
      goto switch_1_12292;
    } else {
        goto switch_1_default;
        if (0) {
          switch_1_12292:
          __goblint_check(1); // reachable

          goto switch_1_break;

          switch_1_default:

          goto end;
        }
          switch_1_break: ;
    }

  }
  while_0_break: ;

  end:
}

int loop2() {
  int r = 0; // variable to prevent CIL simplification
  if (r)
    goto loop2_entry1;
  else
    goto loop2_entry2;

loop2_entry1:
  __goblint_check(1); // reachable
  __goblint_check(1); // reachable
loop2_entry2:
  __goblint_check(1); // reachable
  __goblint_check(1); // reachable
  goto loop2_entry1;

  return 0;
}

int main() {
  int r;
  switch (r) {
    case 0:
      cycle2_1();
      break;
    case 1:
      cycle2_0();
      break;
    case 2:
      loops_s3_min();
      break;
    case 3:
      loop2();
      break;
  }
  return 0;
}