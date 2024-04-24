// SKIP PARAM: --set ana.activated[+] apron --enable annotation.goblint_relation_track --set ana.activated[+] unassume --set witness.yaml.unassume 45-apron-tracked-global-annot.yml
#include <goblint.h>

int g __attribute__((__goblint_relation_track__)) = 0;

int main() {
  __goblint_check(g == 0); // unassume should not crash here
  return 0;
}
