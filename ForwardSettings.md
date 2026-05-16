# Forward Solver Settings

Reference of options introduced for the forward constraint system / solvers, and which solvers each one affects.

Solvers: `fwd` (`fwdSolver.ml`), `bu` (`bu.ml`), `wbu` (`wbu.ml`).

## Constraint-system options

| Option | Default | Applies to | Notes |
|---|---|---|---|
| `solvers.fwd.digests` | `true` | fwd, bu, wbu | Use digests for path-sensitivity in the forward constraint system instead of the traditional `PathSensitive2` lifter. Set in `fwdControl.ml`. |

## Solver options (shared, via `BaseFwdSolver` / `Warrow`)

| Option | Default | Applies to | Notes |
|---|---|---|---|
| `solvers.fwd.update_gas` | `0` | fwd, bu, wbu | How often an incomparable contribution may overwrite the old one before warrowing kicks in. Used in both `SolverLocals` and `SolverGlobals` warrow config. |
| `solvers.fwd.global_gc` | `false` | fwd, bu, wbu | Garbage-collect global contributions that disappear from an unknown's RHS. |
| `solvers.fwd.local_gc` | `false` | fwd, bu, wbu | Same, for local contributions. |

## Solver-specific options

| Option | Default | Applies to | Notes |
|---|---|---|---|
| `solvers.fwd.work_iteration` | `"set"` | fwd, wbu | Worklist iteration strategy: `lifo` (stack) or `set` (set-ordered, minimum first). Does **not** apply to `bu`, which has no worklist (pure recursive `iterate`). |
| `solvers.bu.abort` | `true` | bu, wbu | Suppress propagations from a contributor local that is currently being iterated and has been aborted. Does **not** apply to `fwd`, which has no called/aborted semantics. |

## Quick coverage matrix

| Option | fwd | bu | wbu |
|---|:---:|:---:|:---:|
| `solvers.fwd.digests` | x | x | x |
| `solvers.fwd.update_gas` | x | x | x |
| `solvers.fwd.global_gc` | x | x | x |
| `solvers.fwd.local_gc` | x | x | x |
| `solvers.fwd.work_iteration` | x | – | x |
| `solvers.bu.abort` | – | x | x |

# Research Questions

For simplicity, we fix the update rule related params (gas etc.)

RQ1. Do digests help?
RQ2. Does garbage collection help?
RQ3. Does work_iteration make a difference?
RQ4. Does aborting make a difference?
RQ5. How do the 3 forward solvers compare to one-another and to td_simplified_ref_improved?

# Config file naming scheme

Config files for benchmark runs are named with a fixed positional layout so scripts can parse them by splitting on `_`:

```
<solver>_<digest>_<gc>_<abort>_<iter>.json
```

Field values:
- `<solver>`: `fwd` | `bu` | `wbu`
- `<digest>`: `digest` | `nodigest`
- `<gc>`: `gc` | `nogc` (toggles local + global GC together)
- `<abort>`: `abort` | `noabort` | `na` (`na` for `fwd`, which has no abort)
- `<iter>`: `lifo` | `set` | `na` (`na` for `bu`, which has no worklist)

Examples:
- `fwd_digest_gc_na_set.json`
- `bu_digest_gc_abort_na.json`
- `wbu_nodigest_nogc_noabort_lifo.json`

`update_gas` and other update-rule params are held fixed across runs and not encoded in the filename.
