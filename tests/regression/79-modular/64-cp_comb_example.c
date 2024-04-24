//PARAM: --disable ana.opt.hashcons  --set dbg.timeout 900 --sem.unknown_function.spawn false --sem.unknown_function.call false --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'" --enable ana.sv-comp.functions  --enable allglobs --enable dbg.timing.enabled  --set ana.modular.funs[+] "'foo'" --enable ana.modular.only

// Previously did not terminate because an infinite sequence of addresses with some repeated offset was generated.
// Output for this file when tracing the local states of Base should not contain the following address offsets: .buf[def_exc:Unknown int([-63,63])].buf[def_exc:Unknown int([-63,63])] or .buf.buf
struct a {
  char *b;
  char buf;
};
struct a *d;

void bar(struct a *g, struct a *h) {
  g->b = &g->buf;
  g->b = h->b;
}

int foo() {
  struct a i;
  long l;
  while (1) {
    bar(d + l, &i);
  }
}
