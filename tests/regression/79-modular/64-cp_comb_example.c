//PARAM: --disable ana.opt.hashcons  --set dbg.timeout 900 --sem.unknown_function.spawn false --sem.unknown_function.call false --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'" --enable ana.sv-comp.functions  --enable allglobs --enable dbg.timing.enabled  --trace combine_env_modular_update --trace collect_targets_with_graph --set ana.modular.funs[+] "'knuth_morris_pratt_multibyte'" --enable ana.modular.only  --trace combine_env_modular_basic

// Output for this file should not contain the following address offset: .buf[def_exc:Unknown int([-63,63])].buf[def_exc:Unknown int([-63,63])]
struct a {
  const *b;
  char buf;
} c;
long e;
d(struct a *) { c.b = &c.buf; }
knuth_morris_pratt_multibyte() {
  struct a f;
  d(&f);
  f.b += e;
}
