Autotune barriers:
  $ goblint --enable ana.autotune.enabled --set ana.autotune.activated[+] pthreadBarriers 01-simple.c 2>&1 | grep -c "Barrier initialization -> enabling pthread barrier analysis: \"pthreadBarriers\""
  1
