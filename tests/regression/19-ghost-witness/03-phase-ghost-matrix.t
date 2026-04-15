Run `phaseGhost` on a matrix of ghost variables covering the main cases.

  $ goblint --set ana.activated[+] phaseGhost --enable warn.deterministic --set lib.activated[+] sv-comp --set witness.yaml.extraGhosts[+] ghost_a --set witness.yaml.extraGhosts[+] ghost_b --set witness.yaml.extraGhosts[+] ghost_c --set witness.yaml.extraGhosts[+] ghost_d --set witness.yaml.extraGhosts[+] ghost_e --set witness.yaml.extraGhosts[+] ghost_f --set witness.yaml.extraGhosts[+] ghost_g --set witness.yaml.extraGhosts[+] ghost_h --set witness.yaml.extraGhosts[+] ghost_i --set witness.yaml.extraGhosts[+] ghost_j --set witness.yaml.extraGhosts[+] ghost_k --set witness.yaml.extraGhosts[+] ghost_l --set witness.yaml.extraGhosts[+] ghost_m --set witness.yaml.extraGhosts[+] ghost_n --set witness.yaml.extraGhosts[+] ghost_o --set witness.yaml.extraGhosts[+] ghost_p --set witness.yaml.extraGhosts[+] ghost_q --set witness.yaml.extraGhosts[+] ghost_r --set witness.yaml.extraGhosts[+] ghost_s --set witness.yaml.extraGhosts[+] ghost_t --set witness.yaml.extraGhosts[+] ghost_u --set witness.yaml.extraGhosts[+] ghost_v --set witness.yaml.extraGhosts[+] ghost_w --set witness.yaml.extraGhosts[+] ghost_x --set witness.yaml.extraGhosts[+] ghost_y --set colors never 03-phase-ghost-matrix.c > phase-ghost-matrix.out 2>&1

Unique-thread and increment-by-one successes are reported.

  $ grep -E "phaseGhost: global ghost_a is only accessed by unique thread .* and is only ever increased by one" phase-ghost-matrix.out | sed -E 's/unique thread .*/unique thread <tid> and is only ever increased by one/'
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread <tid> and is only ever increased by one

  $ grep -E "phaseGhost: global ghost_i is only accessed by unique thread .* and is only ever increased by one" phase-ghost-matrix.out | sed -E 's/unique thread .*/unique thread <tid> and is only ever increased by one/'
  [Info][Witness] phaseGhost: global ghost_i is only accessed by unique thread <tid> and is only ever increased by one

Constant-folding from known ghost values is used for non-syntactic `+1` cases too.

  $ grep -E "phaseGhost: global ghost_q is only accessed by unique thread .* and is only ever increased by one" phase-ghost-matrix.out | sed -E 's/unique thread .*/unique thread <tid> and is only ever increased by one/'
  [Info][Witness] phaseGhost: global ghost_q is only accessed by unique thread <tid> and is only ever increased by one

Unique-thread accesses with non-`+1` updates are rejected.

  $ grep -E "phaseGhost: global ghost_b is only accessed by unique thread .* but is not only ever increased by one" phase-ghost-matrix.out | sed -E 's/unique thread .* but/unique thread <tid>, but/'
  [Warning][Witness] phaseGhost: global ghost_b is only accessed by unique thread <tid>, but is not only ever increased by one

  $ grep -E "phaseGhost: global ghost_d is only accessed by unique thread .* but is not only ever increased by one" phase-ghost-matrix.out | sed -E 's/unique thread .* but/unique thread <tid>, but/'
  [Warning][Witness] phaseGhost: global ghost_d is only accessed by unique thread <tid>, but is not only ever increased by one

  $ grep -E "phaseGhost: global ghost_r is only accessed by unique thread .* but is not only ever increased by one" phase-ghost-matrix.out | sed -E 's/unique thread .* but/unique thread <tid>, but/'
  [Warning][Witness] phaseGhost: global ghost_r is only accessed by unique thread <tid>, but is not only ever increased by one

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
