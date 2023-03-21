// PARAM: --set ana.activated[+] expsplit --disable sem.unknown_function.spawn --disable sem.unknown_function.invalidate.globals --disable sem.unknown_function.invalidate.args --enable dbg.verbose --disable exp.volatiles_are_top --enable ana.int.interval
#include<setjmp.h>
jmp_buf env_buffer;

struct c {
  char *g;
} s;

int g;

set_key(struct c* t) {
  char keyword[20];
  keyword[0] = 'a';
  t->g = keyword;

  if (*t->g) { g=1; }
}

main() {
  struct c * t = &s;

  switch(setjmp(env_buffer)) {
    case 0: break;
    case 1:
      if (*t->g) { g=1; }
      // This refinement somehow adds bottom for keyword as an explicit binding (???)
      set_key(t);
      longjmp(env_buffer, 2); //NOWARN
      break;
    case 2:
      return;
  }

  set_key(t);
  g = 4;

  longjmp(env_buffer, 1);
}
