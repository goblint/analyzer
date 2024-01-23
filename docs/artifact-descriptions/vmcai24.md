# Correctness Witness Validation by Abstract Interpretation
## Artifact

This artifact contains everything mentioned in the evaluation section of the paper: Goblint implementation, scripts, benchmarks, manual witnesses and other tools.

**Note to artifact reviewers:** in the smoke test phase, try to only run the performance evaluation since it is very quick compared to the precision evaluation.

## Requirements
* [VirtualBox](https://www.virtualbox.org/).
* 2 CPU cores.
* 8 GB RAM.
* 7 GB disk space.
* ~45min.

## Layout
* `README.md`/`README.pdf` — this file.
* `LICENSE`.
* `unassume.ova` — VirtualBox virtual machine.

  In `/home/vagrant` contains:
  * `goblint/` ­— Goblint with unassume support, including source code.
  * `CPAchecker-2.2-unix/` — CPAchecker from [SV-COMP 2023 archives](https://gitlab.com/sosy-lab/sv-comp/archives-2023).
  * `UAutomizer-linux/` — Ultimate Automizer from [SV-COMP 2023 archives](https://gitlab.com/sosy-lab/sv-comp/archives-2023).
  * `eval-prec/` — precision evaluation (script, benchmarks, manual witnesses).
  * `eval-perf/` — performance evaluation (script, benchmarks, manual witnesses).
  * `results/` — results (initially empty).

* `results/` — evaluation results tables with data used for the paper.

## Reproduction
1. Import the virtual machine into VirtualBox.
2. Start the virtual machine and log in with username "vagrant" (not "Ubuntu"!) and password "vagrant".
3. Right click on the desktop and open Applications → Accessories → Terminal Emulator.

### Precision evaluation
1. Run `./eval-prec/run.sh` in the terminal emulator. This takes ~42min.
2. Run `firefox results/eval-prec/table-generator.table.html` to view the results.

   The HTML table contains the following status columns (cputime, walltime and memory can be ignored):
   1. Goblint w/o witness (true means verified).
   2. Goblint w/ manual witness (true means witness validated).
   3. Goblint w/ witness from CPAchecker (true means program verified with witness-guidance).
   4. Goblint w/ witness from CPAchecker (true means witness validated).
   5. Goblint w/ witness from UAutomizer (true means program verified with witness-guidance).
   6. Goblint w/ witness from UAutomizer (true means witness validated).

   Table 1 in the paper presents these results, except the rows are likely in a different order.

### Performance evaluation
1. Run `./eval-perf/run.sh` in the terminal emulator. This takes ~30s.
2. Run `firefox results/eval-perf/table-generator.table.html` to view the results.

   The HTML table contains the following relevant columns (others can be ignored):
   1. Goblint w/o witness, evals.
   2. Goblint w/o witness, cputime.
   3. Goblint w/ manual witness, evals.
   4. Goblint w/ manual witness, cputime.

   Table 2 in the paper presents these results, except the rows are likely in a different order.


## Goblint implementation
[Goblint](https://github.com/goblint/analyzer) is an open source static analysis framework for C.
Goblint itself is written in OCaml.
Being open source, it allows existing implementations of analyses and abstract domains to be reused and modified.
As a framework, it also allows new ones to be easily added.
For more details, refer to the linked GitHub repository and related documentation.

Key parts of the code related to this paper are the following:

1. `src/analyses/unassumeAnalysis.ml`: analysis, which emits unassume operation events to other analyses for YAML-witness–guided verification.
2. `src/analyses/base.ml` lines 2551–2641: propagating unassume for non-relational domains of the `base` analysis.
3. `src/analyses/apron/relationAnalysis.apron.ml` lines 668–693: strengthening-based dual-narrowing unassume for relational Apron domains of the `apron` analysis.
4. `src/cdomains/apron/apronDomain.apron.ml` lines 625–679: strengthening operator used for dual-narrowing of Apron domains.
5. `src/util/wideningTokens.ml`: analysis lifter that adds widening tokens for delaying widenings from unassuming.
6. `src/witness/yamlWitness.ml` lines 398–683: YAML witness validation.
