Test that warning is issued when hashconsing is disabled but ARG is enabled:

  $ goblint --disable ana.opt.hashcons --enable exp.arg.enabled 52-hashcons-warning.c 2>&1 | grep -i "hashcons"
  [Warning] Disabling ana.opt.hashcons has no effect because hashconsing is implicitly enabled by exp.arg.enabled

Test that warning is issued when hashconsing is disabled but Apron is enabled:

  $ goblint --disable ana.opt.hashcons --set ana.activated[+] apron 52-hashcons-warning.c 2>&1 | grep -i "hashcons"
  [Warning] Disabling ana.opt.hashcons has no effect because hashconsing is implicitly enabled by Apron (ana.activated includes 'apron')

Test that no warning is issued when hashconsing is enabled with ARG:

  $ goblint --enable ana.opt.hashcons --enable exp.arg.enabled 52-hashcons-warning.c 2>&1 | grep -i "hashcons"
  [1]

Test that no warning is issued when hashconsing is enabled with Apron:

  $ goblint --enable ana.opt.hashcons --set ana.activated[+] apron 52-hashcons-warning.c 2>&1 | grep -i "hashcons"
  [1]

Test that no warning is issued when hashconsing is disabled without ARG or Apron:

  $ goblint --disable ana.opt.hashcons 52-hashcons-warning.c 2>&1 | grep -i "hashcons"
  [1]
