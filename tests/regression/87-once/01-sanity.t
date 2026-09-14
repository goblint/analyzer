Autotune once:
  $ goblint --enable ana.autotune.enabled --set ana.autotune.activated[+] pthreadOnce 01-sanity.c 2>&1 | grep -c "Once usage -> enabling pthread once analysis: \"pthreadOnce\""
  1
