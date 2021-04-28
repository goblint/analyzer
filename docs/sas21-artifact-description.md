# SAS21 Artifact Description

## Validation

### Step by Step Instructions
TODO: update for Docker container running and paths

1. Run the script `./update_suite_traces.rb`. This takes ~20 min (see note below).
2. Open the results HTML `./bench_result/index.html`.
3. Validate results with the paper:
    1. The table cells give analysis time in seconds for each benchmark (along the left) and each analysis (along the top). These are illustrated by Figure 2 in the paper.

        The mapping between analysis names in the paper and in the results is given by the following table:
        | Name in paper    | Name in results |
        | ---------------- | --------------- |
        | Protection-Based | `protection`    |
        | Miné             | `mine-W`        |
        | Lock-Centered    | `lock`          |
        | Write-Centered   | `write`         |
        | Combined         | `write+lock`    |

    2. The last column of the table links to automaton pairwise precision comparison output of the analyses for each benchmark. These are described in Section 5 in the paper.

    3. The last number (after `=`) in parenthesis in the table cells gives the logical LoC. These are mentioned in Section 5 in the paper.

        The column "Size" of the table only gives physical LoC, which includes excessive struct and extern function declarations.


### Notes
* The source code for benchmarks can be found in `./pthread/` and `./svcomp/`.
* Although the it takes ~20 min to run all the benchmarks, the script continually updates the results HTML. Therefore it's possible to observe the first results in the partially-filled table without having to wait for the script to finish.


## Extension

### Implementation of Analyses in the Paper
The OCaml source code for the analyses is found in `./src/analyses/basePriv.ml`.
Each one is an appropriately-named module, e.g. `ProtectionBasedPriv`, with the following members:
* The inner module `D` defines the local domain (components), except σ. Rest of the implementation uses `st.priv` to access the `D` components and `st.cpa` to aggess the σ component.
* The inner module `G` defines the global domain, while the global constraint variables are always fixed to be program variables. Hence, for example for Protection-Based, the global constraint variables [g] and [g]' in the paper are implemented by a pair of values under `g`.
* The function `startstate` defines "init" for `D`.
* The functions `read_global` and `write_global` define "x = g" and "g = x" respectively. These implicitly include the surrounding "lock(m_g)" and "unlock(m_g)".
* The functions `lock` and `unlock` define "lock(a)" and "unlock(a)" respectively.
* The function `threadenter` defines the side-effected initial state for u_1 in "x = create(u_1)".
* The remaining functions `enter_multithreaded`, `escape` and `sync` implement other Goblint features necessary to soundly analyze actual C programs.

Besides the five analyses presented in the paper, the `basePriv.ml` file already contains a handful of other experimental implementations, showing that the framework and its thread-modularity is extensible and not at all limited to the five analyses.

### Step by Step Instructions
TODO: update for Docker container running and paths

1. Modify or add thread-modular analyses in `./src/analyses/basePriv.ml`.
2. Run `make`.
3. Observe updated behavior, either:
    * Re-run the benchmarking as described above under Validation.

        or

    * Run the script `./scripts/privPrecCompare.sh --conf conf/traces.json ../bench/pthread/ypbind_comb.c` to run the analyses and their comparison on a single C program with the Goblint configuration file used for this evaluation.

### Outline of how the code is structured

The source code is in the directory src, where the subdirectories are structured as follows:

The most relevant directories are:
- src/solvers: Different fix-point solvers that can be used by Goblint (default is TD3)
- src/domains: Various generic abstract domains: Sets, Maps, ...
- src/cdomains: Abstract domains for C programs (e.g. Integers, Addresses, ...)
- src/analyses: Different analyses supported by Goblint
- src/framework: The code of the analysis framework

Other, not directly relevant, directories:
- src/extract: Related to extracting Promela models from C code
- src/incremental: Related to Incremental Analysis
- src/spec: Related to parsing Specifications for an automata-based analysis of liveness properties
- src/transform: Specify transformations to run based on the analysis results
- src/util: Various utility modules
- src/witness: Related to witness generation

