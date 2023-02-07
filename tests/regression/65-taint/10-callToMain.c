// PARAM: --disable ana.base.context.non-ptr --set ana.activated[+] taintPartialContexts
#include <goblint.h>

int glob;

int main(int argc, char **argv) {
  char **other= malloc(1);

  if(argc < 0) {
    return 0;
  } else {
    glob = 10;
    main(-1, other);
    glob = -10;
    main(-1, other);

    __goblint_check(glob < 0);
  }
}
