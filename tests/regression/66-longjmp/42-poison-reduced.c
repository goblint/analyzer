// PARAM: --set ana.activated[+] expsplit --disable sem.unknown_function.spawn --disable sem.unknown_function.invalidate.globals --disable sem.unknown_function.invalidate.args --enable dbg.verbose --disable exp.volatiles_are_top --enable ana.int.interval
#include<setjmp.h>
jmp_buf env_buffer;
struct c {
  char *g;
};

int u(struct c * t) {
  if (*t->g) {
   return 2;
  } else {
   return 3;
  }
}

void set_g_to_keyword(struct c* t) {
  char keyword[20];
  keyword[0] = 'a';
  t->g = keyword;
}

main() {
  struct c* ab = malloc(sizeof(struct c));
  int x;

  if(setjmp(env_buffer)) {
   __goblint_check(x == 2);
   set_g_to_keyword(ab);
  }
  else {
   set_g_to_keyword(ab);
   x = 1;
   u(ab);
   x = 2;
   longjmp(env_buffer, 1);
  }
}
