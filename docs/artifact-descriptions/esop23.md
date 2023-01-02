# ESOP '23 Artifact Description

This is the artifact description for our ESOP '23 paper "Clustered Relational Thread-Modular Abstract Interpretation with Local Traces".
<!-- The artifact is available from Zenodo [here](TODO: Update URL). -->

The artifact is a VirtualBox Image based on Ubuntu 22.04.1. The login is `goblint:goblint`.

<!-- If you are reading these instructions on goblint.readthedocs.io, they might have been updated to match the current version of Goblint.
When using the artifact, follow the similar instructions it includes. -->

# Getting Started Guide

- Install VirtualBox (e.g. from [virtualbox.org](https://www.virtualbox.org/))
- Import the VM via `File -> Import Appliance`
- Start the VM and (if prompted) login as `goblint` with password `goblint`
- Navigate to the folder `~/analyzer`. All paths are given relative to it.
- Run the following commands to verify the installation works as intended
    - `./scripts/esop23-kick-tires.sh` (will take ~3min)
        - Internally, this will run a few internal regression tests (you can open the script to see which)
        - After the command has run, there should be some messages `No errors :)` as well as some messages `Excellent: ignored check on ... now passing!`)

# Step-by-Step Instructions

The following are step-by-step instructions to reproduce the experimental results underlying the paper.
Depending on the host machine, the run times will be slightly different from what is reported in the paper,
but they should behave the same way relative to each other.

**Important note: We based our implementation on our previous work on Goblint, but also compare with **

## Claims in Paragraph "Internal comparison" (p.23)

All these claims derive from Fig. 13 (a) and 13 (b). The data underlying these tables is produced by running:

1. Run the script `../bench/esop23_fig13.rb`. This takes ~25 min (see note below).
2. Open the results HTML `../bench/bench_result/index.html`.

    - The configurations are named the same as in the paper (with the exception that the `Interval` configuration from the paper is named `box` in the table).
    - There are some slight deviations between the `LLoCs` in the paper and the artifact, that are due
      to different versions of library functions caused by different versions of `glibc`.
    - The number of threads and which are unique is given by the numbers following `T:` in the parenthesis after the runtimes



### Notes
* The source code for benchmarks can be found in `../bench/pthread/` and `../bench/svcomp/`.
* Although it takes ~25 min to run all the benchmarks, the script continually updates the results HTML. Therefore it's possible to observe the first results in the partially-filled table without having to wait for the script to finish.


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
