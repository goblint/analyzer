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

