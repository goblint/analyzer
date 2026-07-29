Run `phaseGhost` on a matrix of ghost variables covering the main cases.

  $ goblint --set ana.activated[+] phaseGhost --enable warn.deterministic --set lib.activated[+] sv-comp --set witness.yaml.validate 03-phase-ghost-matrix.yml --set colors never 03-phase-ghost-matrix.c > phase-ghost-matrix.out 2>&1

Unique-thread and monotone bounded successes are reported.

  $ grep -E "phaseGhost: global ghost_a is only accessed by unique thread .* and is monotonically increased to known bounds" phase-ghost-matrix.out | sed -E 's/unique thread .*/unique thread <tid> and is monotonically increased to known bounds/'
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread <tid> and is monotonically increased to known bounds

  $ grep -E "phaseGhost: global ghost_i is only accessed by unique thread .* and is monotonically increased to known bounds" phase-ghost-matrix.out | sed -E 's/unique thread .*/unique thread <tid> and is monotonically increased to known bounds/'
  [Info][Witness] phaseGhost: global ghost_i is only accessed by unique thread <tid> and is monotonically increased to known bounds

Constant-folding from known ghost values is used for non-syntactic `+1` cases too.

  $ grep -E "phaseGhost: global ghost_q is only accessed by unique thread .* and is monotonically increased to known bounds" phase-ghost-matrix.out | sed -E 's/unique thread .*/unique thread <tid> and is monotonically increased to known bounds/'
  [Info][Witness] phaseGhost: global ghost_q is only accessed by unique thread <tid> and is monotonically increased to known bounds

Unique-thread accesses with bounded jumps larger than one are accepted.

  $ grep -E "phaseGhost: global ghost_b is only accessed by unique thread .* and is monotonically increased to known bounds" phase-ghost-matrix.out | sed -E 's/unique thread .*/unique thread <tid> and is monotonically increased to known bounds/'
  [Info][Witness] phaseGhost: global ghost_b is only accessed by unique thread <tid> and is monotonically increased to known bounds

  $ grep -E "phaseGhost: global ghost_d is only accessed by unique thread .* and is monotonically increased to known bounds" phase-ghost-matrix.out | sed -E 's/unique thread .*/unique thread <tid> and is monotonically increased to known bounds/'
  [Info][Witness] phaseGhost: global ghost_d is only accessed by unique thread <tid> and is monotonically increased to known bounds

  $ grep -E "phaseGhost: global ghost_r is only accessed by unique thread .* and is monotonically increased to known bounds" phase-ghost-matrix.out | sed -E 's/unique thread .*/unique thread <tid> and is monotonically increased to known bounds/'
  [Info][Witness] phaseGhost: global ghost_r is only accessed by unique thread <tid> and is monotonically increased to known bounds

Unique-thread accesses with unknown or non-increasing updates are rejected.

  $ grep -E "phaseGhost: global ghost_e is only accessed by unique thread .* but is not monotonically increased to known bounds" phase-ghost-matrix.out | sed -E 's/unique thread .* but/unique thread <tid>, but/'
  [Warning][Witness] phaseGhost: global ghost_e is only accessed by unique thread <tid>, but is not monotonically increased to known bounds

  $ grep -E "phaseGhost: global ghost_n is only accessed by unique thread .* but is not monotonically increased to known bounds" phase-ghost-matrix.out | sed -E 's/unique thread .* but/unique thread <tid>, but/'
  [Warning][Witness] phaseGhost: global ghost_n is only accessed by unique thread <tid>, but is not monotonically increased to known bounds

Accesses from multiple unique threads are rejected.

  $ grep -E "phaseGhost: global ghost_g is accessed by multiple unique threads" phase-ghost-matrix.out | sed -E 's/multiple unique threads: .*/multiple unique threads: <tids>/'
  [Warning][Witness] phaseGhost: global ghost_g is accessed by multiple unique threads: <tids>

  $ grep -E "phaseGhost: global ghost_m is accessed by multiple unique threads" phase-ghost-matrix.out | sed -E 's/multiple unique threads: .*/multiple unique threads: <tids>/'
  [Warning][Witness] phaseGhost: global ghost_m is accessed by multiple unique threads: <tids>

Accesses from a non-unique thread id are rejected.

  $ grep -E "phaseGhost: global ghost_h is accessed by a non-unique or unknown thread id" phase-ghost-matrix.out
  [Warning][Witness] phaseGhost: global ghost_h is accessed by a non-unique or unknown thread id

  $ grep -E "phaseGhost: global ghost_p is accessed by a non-unique or unknown thread id" phase-ghost-matrix.out
  [Warning][Witness] phaseGhost: global ghost_p is accessed by a non-unique or unknown thread id
