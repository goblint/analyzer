// PARAM: --enable ana.int.congruence --enable ana.int.interval
// reduced (via creduce and manually) from sv-benchmarks/c/hardness-nfm22/hardness_codestructure_dependencies_file-70.c

main() {
  int a;
  unsigned c = 1;
  if (a)
    c = 4;
  if (c + (c + 2)) // NOWARN
    a = 1;
}
