# Apron environment issue observed while crafting phase witnesses

While making the `90-phase-crafted` examples more loop-heavy, several variants
crashed before producing an SV-COMP result when run with `phaseGhostSplit`.
The common shape was:

- a worker computes a local value by calling a small helper function,
- the helper contains a finite loop which Goblint unrolls,
- the local returned by that helper is later used at or after a phase-ghost
  split point,
- octagon/Apron domains are enabled by the SV-COMP level04 configuration.

The crash looked like this:

```text
[Error][Analyzer] About to crash at mark transfer function at 03-telemetry-rollup.c:20:7-20:29!
Fatal error: exception Apron.Manager.Error
 {  exn = Exc_invalid_argument; funid = Funid_forget_array;
  msg = unknown variable ok#980 in the environment; }
Marked with transfer function at 03-telemetry-rollup.c:20:7-20:29
```

The triggering version of the telemetry example had this helper-return pattern:

```c
static int fold_even_codes(void) {
  int total = 0;
  for (int code = 2; code <= 8; code += 2)
    total += code / 2;
  return total;
}

void *sampler(void *arg) {
  int ok = fold_even_codes();
  int parity = 0;
  for (int i = 0; i < 5; i++)
    parity ^= (i & 1);

  pthread_mutex_lock(&telemetry_lock);
  /* GHOST sampler 1 */ telemetry[OK] += ok;
  pthread_mutex_unlock(&telemetry_lock);

  pthread_mutex_lock(&telemetry_lock);
  /* GHOST sampler 2 */ telemetry[PARITY] ^= parity;
  pthread_mutex_unlock(&telemetry_lock);
  return 0;
}
```

The command shape which exposed it was:

```sh
../../../goblint \
  --conf ../../../conf/svcomp26/common.json \
  --conf ../../../conf/svcomp26/verify.json \
  --conf ../../../conf/svcomp26/level04.json \
  --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" \
  --enable ana.sv-comp.functions \
  --set ana.base.privatization protection-atomic-ghost \
  --set exp.architecture 64bit \
  --disable witness.yaml.enabled \
  --set ana.path_sens[+] phaseGhostSplit \
  --set witness.yaml.validate 03-telemetry-rollup.yml \
  --set ana.activated[+] phaseGhost \
  --set ana.activated[+] phaseGhostSplit \
  03-telemetry-rollup.c
```

Inlining the helper computation into the worker avoided the crash:

```c
void *sampler(void *arg) {
  int ok = 0;
  for (int code = 2; code <= 8; code += 2)
    ok += code / 2;
  /* rest unchanged */
}
```

Similar crashes appeared in helper-return variants of:

- `02-warehouse-stock.c` with `triangular(3)`,
- `04-cache-statistics.c` with `warm_pages()`,
- `05-reference-lifecycle.c` with `count_bits(15)`,
- `08-sensor-calibration.c` with `offset_from_profile()`,
- `09-resource-pool.c` with `request_size()`,
- `10-sharded-counter.c` with `route(4)`,
- `14-replica-progress.c` with `log_span(1, 4)`,
- `17-network-bookkeeping.c` with `multiplier()`,
- `19-job-scheduler.c` with `dispatch_count()`,
- `20-audit-aggregation.c` with `scan_range(1, 6)`.

The final checked-in regression programs keep the loops, but avoid the
helper-return form so that the suite exercises phase witnesses instead of this
Apron/environment crash path.
