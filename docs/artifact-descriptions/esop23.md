# ESOP '23 Artifact Description

This is the artifact description for our ESOP '23 paper "Clustered Relational Thread-Modular Abstract Interpretation with Local Traces".
The artifact is available from Zenodo at [https://doi.org/10.5281/zenodo.7505428](https://doi.org/10.5281/zenodo.7505428). The `md5` of the `.ova` file is `3f6a5af9fe94cb377c397758b4941b15`.
The associated source code can conveniently be browsed at: https://github.com/goblint/analyzer/tree/esop23.

The artifact is a VirtualBox Image based on Ubuntu 22.04.1. The login is `goblint:goblint`.

For convenience this file is also included in the VM (at `~/analyzer/docs/artifact-descriptions/esop23.md`) in order to be able to copy commands.

<!-- If you are reading these instructions on goblint.readthedocs.io, they might have been updated to match the current version of Goblint.
When using the artifact, follow the similar instructions it includes. -->

# Getting Started Guide

- Install VirtualBox (e.g. from [virtualbox.org](https://www.virtualbox.org/))
- Import the VM via `File -> Import Appliance` (requires ~25GB of free memory)
- Start the VM and (if prompted) login as `goblint` with password `goblint`
- If you use a Non-English keyboard layout, you can change the keyboard layout in the top right corner to match yours.
- Navigate to the folder `~/analyzer`. All paths are given relative to it.
- Run the following commands to verify the installation works as intended
    - `./scripts/esop23-kick-tires.sh` (will take ~1min)
        - Internally, this will run a few internal regression tests (you can open the script to see which). These exercise the major parts of the system.
        - After the command has run, there should be some messages `No errors :)` as well as some messages `Excellent: ignored check on ... now passing!`
        <details>
            <summary>Detailed expected message</summary>
            ```
            Excellent: ignored check on tests/regression/01-cpa/33-asserts.c:35 is now passing!
            Excellent: ignored check on tests/regression/28-race_reach/22-deref_read_racefree.c:20 is now passing!
            No errors :)
            No errors :)
            Excellent: ignored check on 21-traces-cluster-based.c:48 is now passing!
            Excellent: ignored check on 21-traces-cluster-based.c:66 is now passing!
            Excellent: ignored check on 21-traces-cluster-based.c:69 is now passing!
            Excellent: ignored check on 22-traces-write-centered-vs-meet-mutex.c:25 is now passing!
            Excellent: ignored check on 42-threadenter-arg.c:6 is now passing!
            No errors :)
            No errors :)
            No errors :)
            ```
        </details>

# Step-by-Step Instructions

The following are step-by-step instructions to reproduce the experimental results underlying the paper.
Depending on the host machine, the run times will be slightly different from what is reported in the paper, but they should behave the same way relative to each other.
The runtimes given here are based on runtimes we had in the VM on a host system with an 11th Gen Intel(R) Core(TM) i7-1165G7 @ 2.80GHz with 32GB of RAM with some generous upwards rounding.

**Important note: We based our implementation on our previous work on Goblint, but also compare with Goblint in the non-relational setting from our previous SAS paper. This previous version is referred to as "Goblint w/ Interval"**

**As a first step, navigate to the folder `~/analyzer`. All paths are given relative to it.**

## Claims in Paragraph "Internal comparison" (p.23)

All these claims derive from Fig. 13 (a) and 13 (b). The data underlying these tables is produced by running:

1. Run the script `../bench/esop23_fig13.rb`. This takes ~25min (see note below). The runs for `ypbind` will fail (see below).
2. Open the results HTML `../bench/esop23_fig13/index.html`.

    - The configurations are named the same as in the paper (with the exception that the `Interval` configuration from the paper is named `box` in the table, and `Clusters` is named `cluster12`).
    - As noted in appendix I.1, we had to exclude `ypbind` from the benchmarks, as it spawns a thread from an unknown pointer which the analysis can not handle
    - As dumping the precision information incurs a significant performance penalty, each configuration is run twice: Once to measure runtime and once with
    the post-processing that marshals the internal data structures to disk.



### Notes
* The source code for benchmarks can be found in `../bench/pthread/` and `../bench/svcomp/`.
* Although it takes ~25 min to run all the benchmarks, the script continually updates the results HTML. Therefore it's possible to observe the first results in the partially-filled table without having to wait for the script to finish (if that shows you a blank, try waiting a while and refreshing).


## All other claims in Section 9

All these claims are based on the contents of Table 2.

### Reproducing the tables for our tool & Goblint w/ Interval

To generate the tables for all sets, run `./scripts/esop23-table2.sh` (will take ~20 min).

This will produce one HTML file with results per group:

| Set      | HTML-File                                       |
| -------- | ------------------------------------------------|
| Our      | `../bench/esop23_table2_set_our/index.html`     |
| Goblint  | `../bench/esop23_table2_set_goblint/index.html` |
| Watts    | `../bench/esop23_table2_set_watts/index.html`   |
| Ratcop   | `../bench/esop23_table2_set_ratcop/index.html`  |


How to interpret the results tables:
  - The configurations are named the same as in the paper (with the exception that the `Interval` configuration from the paper is named `box` in the table, and `Clusters` is named `cluster12`).
  - The results table shows for each test the total numbers of asserts and how many could be proven:
      - If all are proven, the cell shows a checkmark
      - If none are proven, the cell shows a cross
      - If only some are proven, the cell shows both numbers

**For the Set "Watts":**
 - For a detailed discussion on these benchmarks, see Appendix I.2 of the paper.
 - As opposed to the other scripts, this one also prints run-times as these are needed to also verify **Table 4** in the supplementary material.

Note: To regenerate just some of the results: Invoke one of `../bench/esop23_table2_set_{our,goblint,watts,ratcop}.rb`.

### Reproducing Duet numbers

This artifact ships Duet (at commit `5ea68373bb8c8cff2a9b3a84785b12746e739cee`) with a bug fix (courtesy of its original author Zach Kincaid) allowing it to run successfully on some of the benchmarks.
For others, it still reported a number of reachable asserts that is too low.
We only give the instructions to reproduce the successful runs here. For a detailed discussion see Apppendix I.3.

To generate the CSV file for all sets, invoke `./scripts/esop23-table2-duet.sh` (runs about 2mins).

- The output is the complete output for Duet, which can be quite verbose.
- Outputs `Error: NN` indicate that `NN` asserts could not be proven by Duet and are not indicative of bigger issues.
Also, for some runs Duet will crash with an exception from solver.ml.


| Set      | HTML-File                                       |
| -------- | ------------------------------------------------|
| Our      | `../bench/traces-relational-duet-ours.csv`      |
| Goblint  | -                                               |
| Watts    | `../bench/traces-relational-duet-watts.csv`     |
| Ratcop   | `../bench/traces-relational-duet-ratcop.csv`    |

These files contain for the tests on which Duet did not crash the number of verified assertions,
followed by the number of failed assertions.

Note: To regenerate just some of the results: Invoke one of `python3 ../bench/duet-{ours,goblint,watts,ratcop}.py`.


## Additional Information
### Outline of how the code is structured
Lastly, we give a general outline of how code in the Goblint framework is organized:
The source code is in the directory `./src`, where the subdirectories are structured as follows:

The most relevant directories are:

- `./src/solvers`: Different fix-point solvers that can be used by Goblint (default is TD3)
- `./src/domains`: Various generic abstract domains: Sets, Maps, ...
- `./src/cdomains`: Abstract domains for C programs (e.g. Integers, Addresses, ...)
- `./src/analyses`: Different analyses supported by Goblint
- `./src/framework`: The code of the analysis framework

Other, not directly relevant, directories:

- `./src/extract`: Related to extracting Promela models from C code
- `./src/incremental`: Related to Incremental Analysis
- `./src/spec`: Related to parsing Specifications for an automata-based analysis of liveness properties
- `./src/transform`: Specify transformations to run based on the analysis results
- `./src/util`: Various utility modules
- `./src/witness`: Related to witness generation
