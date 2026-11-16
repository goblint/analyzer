// NONTERM PARAM: --set "ana.activated[+]" termination --set 'ana.activated[+]' branchSeq --set ana.branchSeq.length 3 --enable ana.int.congruence --enable ana.int.bitfield
// From https://github.com/goblint/analyzer/issues/2336#issuecomment-6911574375.

int main(void) {
  int i = 0;
  while (1) // NONTERMLOOP
    i = i ? 0 : 1;
}
