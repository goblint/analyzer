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
    - `./scripts/esop23-kick-tires.sh`
        - Internally, this will run a few internal regression tests (the commands that are run are printed)
        - After the command has run, there should be some messages `No errors :)` as well as some messages `Excellent: ignored check on ... now passing!`)




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
