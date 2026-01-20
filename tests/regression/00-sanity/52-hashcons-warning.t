Test that warning is issued when hashconsing is disabled but ARG is enabled:

  $ goblint --disable ana.opt.hashcons --enable exp.arg.enabled 52-hashcons-warning.c 2>&1 | grep -i "hashcons"
  [Warning] Hashconsing (ana.opt.hashcons) is disabled, but is implicitly enabled because ARG is enabled (exp.arg.enabled)

Test that warning is issued when hashconsing is disabled but Apron is enabled:

  $ goblint --disable ana.opt.hashcons --set ana.activated[+] apron 52-hashcons-warning.c 2>&1 | grep -i "hashcons"
  [Warning] Hashconsing (ana.opt.hashcons) is disabled, but may be required for Apron domain (ana.activated includes 'apron')

Test that no warning is issued when hashconsing is enabled with ARG:

  $ goblint --enable ana.opt.hashcons --enable exp.arg.enabled 52-hashcons-warning.c 2>&1 | grep -i "hashcons"
  [1]

Test that no warning is issued when hashconsing is enabled with Apron:

  $ goblint --enable ana.opt.hashcons --set ana.activated[+] apron 52-hashcons-warning.c 2>&1 | grep -i "hashcons"
  [1]

Test that no warning is issued when hashconsing is disabled without ARG or Apron:

  $ goblint --disable ana.opt.hashcons 52-hashcons-warning.c 2>&1 | grep -i "hashcons"
  [1]
