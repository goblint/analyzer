Table 3: use one configuration for the C and ordinary preprocessed inputs.
Verification without a witness must remain unknown. Ghost-only witnesses,
witnesses with additional invariants, and those invariants with unassume must
all validate. Check every validation counter so ignored entries cannot pass.

  $ analyze () {
  >   goblint --conf ../../../conf/svcomp26/common.json \
  >     --conf ../../../conf/svcomp26/verify.json \
  >     --conf ../../../conf/svcomp26/level00.json \
  >     --set ana.base.privatization protection-atomic-ghost \
  >     --set ana.specification 'CHECK( init(main()), LTL(G ! call(reach_error())) )' \
  >     --set exp.architecture 64bit \
  >     --set 'ana.activated[+]' phaseGhost \
  >     --set 'ana.activated[+]' phaseGhostSplit \
  >     --set 'ana.path_sens[+]' phaseGhostSplit \
  >     --set 'ana.autotune.activated[-]' loopUnrollHeuristic \
  >     --set 'witness.yaml.invariant-types[+]' location_invariant \
  >     --set 'pre.cppflags[+]' -DGOBLINT_NO_BSEARCH \
  >     --set 'pre.cppflags[+]' -DGOBLINT_NO_QSORT \
  >     --disable witness.yaml.enabled --enable witness.yaml.strict \
  >     --enable warn.deterministic "$@" > table3.log 2>&1 || { cat table3.log; return 1; }
  >   awk '/^SV-COMP result:/ { print }
  >        /^  (confirmed|unconfirmed|refuted|error|unchecked|unsupported|disabled):/ { printf "%s %s; ", $1, $2 }
  >        /^  total validation entries:/ { print "total: " $4 }' table3.log
  > }

  $ check () {
  >   input=$1
  >   witness=$2
  >   echo "$input: verification without witness"
  >   analyze "$input" || return
  >   echo "$input: validation"
  >   analyze "$input" --set witness.yaml.validate "$witness.yml" || return
  >   echo "$input: validation with invariants"
  >   analyze "$input" --set witness.yaml.validate "$witness-invariants.yml" || return
  >   echo "$input: validation with invariants and unassume"
  >   analyze "$input" --set witness.yaml.validate "$witness-invariants.yml" \
  >     --set witness.yaml.unassume "$witness-invariants.yml" \
  >     --set 'ana.activated[+]' unassume \
  >     --enable ana.unassume.ghost --enable ana.unassume.precheck
  > }

  $ check 19-stateful01-2.c 19-stateful01-2
  19-stateful01-2.c: verification without witness
  SV-COMP result: unknown
  19-stateful01-2.c: validation
  SV-COMP result: true
  confirmed: 0; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 0
  19-stateful01-2.c: validation with invariants
  SV-COMP result: true
  confirmed: 2; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 2
  19-stateful01-2.c: validation with invariants and unassume
  SV-COMP result: true
  confirmed: 2; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 2

  $ check 19-stateful01-2.i 19-stateful01-2-i
  19-stateful01-2.i: verification without witness
  SV-COMP result: unknown
  19-stateful01-2.i: validation
  SV-COMP result: true
  confirmed: 0; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 0
  19-stateful01-2.i: validation with invariants
  SV-COMP result: true
  confirmed: 2; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 2
  19-stateful01-2.i: validation with invariants and unassume
  SV-COMP result: true
  confirmed: 2; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 2

  $ check 20-race-1_1-join.c 20-race-1_1-join
  20-race-1_1-join.c: verification without witness
  SV-COMP result: unknown
  20-race-1_1-join.c: validation
  SV-COMP result: true
  confirmed: 0; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 0
  20-race-1_1-join.c: validation with invariants
  SV-COMP result: true
  confirmed: 4; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 4
  20-race-1_1-join.c: validation with invariants and unassume
  SV-COMP result: true
  confirmed: 4; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 4

  $ check 20-race-1_1-join.i 20-race-1_1-join-i
  20-race-1_1-join.i: verification without witness
  SV-COMP result: unknown
  20-race-1_1-join.i: validation
  SV-COMP result: true
  confirmed: 0; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 0
  20-race-1_1-join.i: validation with invariants
  SV-COMP result: true
  confirmed: 4; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 4
  20-race-1_1-join.i: validation with invariants and unassume
  SV-COMP result: true
  confirmed: 4; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 4

  $ check 22-pthread-demo-datarace.c 22-pthread-demo-datarace
  22-pthread-demo-datarace.c: verification without witness
  SV-COMP result: unknown
  22-pthread-demo-datarace.c: validation
  SV-COMP result: true
  confirmed: 0; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 0
  22-pthread-demo-datarace.c: validation with invariants
  SV-COMP result: true
  confirmed: 3; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 3
  22-pthread-demo-datarace.c: validation with invariants and unassume
  SV-COMP result: true
  confirmed: 3; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 3

  $ check 22-pthread-demo-datarace.i 22-pthread-demo-datarace-i
  22-pthread-demo-datarace.i: verification without witness
  SV-COMP result: unknown
  22-pthread-demo-datarace.i: validation
  SV-COMP result: true
  confirmed: 0; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 0
  22-pthread-demo-datarace.i: validation with invariants
  SV-COMP result: true
  confirmed: 3; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 3
  22-pthread-demo-datarace.i: validation with invariants and unassume
  SV-COMP result: true
  confirmed: 3; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 3

  $ check 23-arithmetic-prog-ok.c 23-arithmetic-prog-ok
  23-arithmetic-prog-ok.c: verification without witness
  SV-COMP result: unknown
  23-arithmetic-prog-ok.c: validation
  SV-COMP result: true
  confirmed: 0; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 0
  23-arithmetic-prog-ok.c: validation with invariants
  SV-COMP result: true
  confirmed: 3; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 3
  23-arithmetic-prog-ok.c: validation with invariants and unassume
  SV-COMP result: true
  confirmed: 3; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 3

  $ check 23-arithmetic-prog-ok.i 23-arithmetic-prog-ok-i
  23-arithmetic-prog-ok.i: verification without witness
  SV-COMP result: unknown
  23-arithmetic-prog-ok.i: validation
  SV-COMP result: true
  confirmed: 0; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 0
  23-arithmetic-prog-ok.i: validation with invariants
  SV-COMP result: true
  confirmed: 3; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 3
  23-arithmetic-prog-ok.i: validation with invariants and unassume
  SV-COMP result: true
  confirmed: 3; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 3

  $ check 25-test-easy8.c 25-test-easy8
  25-test-easy8.c: verification without witness
  SV-COMP result: unknown
  25-test-easy8.c: validation
  SV-COMP result: true
  confirmed: 0; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 0
  25-test-easy8.c: validation with invariants
  SV-COMP result: true
  confirmed: 4; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 4
  25-test-easy8.c: validation with invariants and unassume
  SV-COMP result: true
  confirmed: 4; unconfirmed: 0; refuted: 0; error: 0; unchecked: 0; unsupported: 0; disabled: 0; total: 4
