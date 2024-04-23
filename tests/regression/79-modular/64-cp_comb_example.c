//PARAM: --disable ana.opt.hashcons  --set dbg.timeout 900 --sem.unknown_function.spawn false --sem.unknown_function.call false --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'" --enable ana.sv-comp.functions  --enable allglobs --enable dbg.timing.enabled  --set ana.modular.funs[+] "'knuth_morris_pratt_multibyte'" --enable ana.modular.only --trace base
// --trace combine_env_modular_update --trace collect_targets_with_graph  --trace combine_env_modular_basic

// Output for this file should not contain the following address offset: .buf[def_exc:Unknown int([-63,63])].buf[def_exc:Unknown int([-63,63])]
struct a *d;
long e, f;
struct a {
  const char *b;
  char buf;
};
struct a c(struct a *g, struct a *h) {
  g->b = &g->buf;
  g = (struct a*) h->b;
}
int knuth_morris_pratt_multibyte() {
  struct a i;
  while (1) {
    c(d + f, &i);
    i.b += e;
  }
}
