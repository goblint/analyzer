// NONTERM PARAM: --set "ana.activated[+]" termination --set 'ana.activated[+]' branchSeq --set ana.branchSeq.length 4 --enable ana.int.congruence --enable ana.int.bitfield

int main(void) {
  int i = 0;
  while (1) // NONTERMLOOP
    if (i == 0)
      i = 1;
    else if (i == 1)
      i = 2;
    else if (i == 2)
      i = 3;
    else
      i = 0;
}
