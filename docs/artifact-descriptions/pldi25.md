# PLDI '25 Artifact Description
## Taking out the Toxic Trash: Recovering Precision in Mixed Flow-Sensitive Static Analyses

This is the artifact description for our PLDI '25 paper "Taking out the Toxic Trash: Recovering Precision in Mixed Flow-Sensitive Static Analyses" by F. Stemmler, M. Schwarz, J. Erhard, S. Tilscher, and H. Seidl.

The artifact is available on [Zenodo](https://zenodo.org/records/15245617).

**The description here is provided for convenience and not maintained.**
The artifact contains Goblint at `pldi25_eval_runtime` and `pldi25_eval_stats` git tags.
The source code repository is available at <https://github.com/tum-cit-pl/precision-recovery-mixed-flowsens-benchmarks>.

For convenience, this file is also included in the VM (at `~/precision-recovery-mixed-flowsens-benchmarks/README.md`) to be able to copy commands.

## Overview

The artifact is a virtual machine packaged for Oracle VirtualBox 6.1, which is available from [Oracle](https://www.virtualbox.org/) and through the package manager of most distributions. The operating system is Ubuntu 24.04 LTS.

The folder structure of this artifact (`~/precision-recovery-mixed-flowsens-benchmarks`) is as follows:

- `analyzer`: the Goblint analyzer
- `helper-scripts`: Some helper scripts
- `rq1-3-benchmarks`: Contains the instructions to reproduce the evaluation for **RQ1-RQ3**
- `rq4-results`: Contains the raw data for **RQ4**
- `rq5-benchmarks`: Contains the instructions to reproduce the evaluation for **RQ5**
- `README.md`: top-level readme
- `CODE.md`: A description of the changes made to Goblint.
- `EXAMPLE.md`: An example program highlighting precision differences

There are two branches of the `analyzer` repository of relevance here:

- `pldi25_eval_runtime` used to measure runtimes.
- `pldi25_eval_stats` collects additional statistics but is slower.

In the artifact, `analyzer` is already checked out and dependencies are installed. If recreating the artifact from scratch proceed to **General setup of the analyzer** at the bottom.

## Requirements

* [VirtualBox](https://www.virtualbox.org/)
* At least 2 CPU cores (more recommended for full reproduction)
* At least 8 GB RAM (more recommended for full reproduction)
* Several GB of disk space
* Up to several days for full reproduction (depending on configuration)

## Getting Started

- For help importing the virtual machine, see <https://docs.oracle.com/cd/E26217_01/E26796/html/qs-import-vm.html>
- The login is `goblint:goblint`.
- **Users not using EN-US keyboard**: Switch the keyboard layout to match yours by
    - Clicking `en` in the right upper corner and
    - Choosing your layout if suggested or clicking on `Keyboard Settings` to set up another.
- Verify the analyzer works by
    - Navigating to the folder of the artifact (`~/precision-recovery-mixed-flowsens-benchmarks`)
    - `cd analyzer`
    - `make sanitytest` (it may run a few minutes)
    - Expected output:

    ```
    Excellent: ignored check on tests/regression/03-practical/21-pfscan_combine_minimal.c:21 is now passing!
    Excellent: ignored check on tests/regression/03-practical/21-pfscan_combine_minimal.c:29 is now passing!
    No errors :)
    ```

## Step-by-Step Reproduction

The steps for reproduction and claims are split by **RQs**, as is the evaluation section:

- For **RQ1-3**, follow the instructions below and in `./rq1-3-benchmarks/README.md`
- For **RQ4**, see below and `./rq4-results/README.md`
- For **RQ5**, follow the instructions below and in `./rq5-benchmarks/README.md`

### Reproduction of Experiments for RQ1-3

The folder structure for this RQ (`~/precision-recovery-mixed-flowsens-benchmarks/rq1-3-benchmarks`) is as follows:

- `paper-runs`: Folder containing the raw data of the runs underlying the plots in the paper
- `plots`: Tex files with pgplots plots for plotting results
- `bench.txt`: Specifies the full list of benchmark programs
- `reduced.txt`: Specifies the reduced list of benchmark programs recommended for reproduction
- Various Ruby / Python / Shell scripts

There are several levels of reproduction possible here:

- **A**: The folder `paper-runs` contains the data produced by the runs we used for the paper. This allows reproducing the plots and data from our raw results. **(Recommended)**
- **B**: We provide a subset of the benchmarks (including the ones used for Plot 3) for which results can realistically be reproduced inside the VM **(Recommended)**
- **C**: We provide the scripts for a full reproduction (all 11k SV-COMP programs). This requires a machine with a considerable number `N` of cores so runtime is acceptable, e.g., 40 for the config and at least `15GB*N` of RAM and a runtime of several days (assuming N=48 cores). **(Not Recommended)**

In the artifact, `sv-comp` is already checked out at the correct path. If recreating the artifact from scratch proceed to **General setup of SV-COMP** at the bottom.

#### Step 1: Producing the raw data

First step: Change into the analyzer repo `cd ..`, run `git checkout pldi25_eval_runtime`, and `make release`.
Then, change back to this directory.

The raw data here takes the form of a text file `svcomp-raw.txt`. The folder `paper-runs` contains the version of this raw data corresponding to the run in the paper.

##### Option A

As our raw data is used, nothing is to be done.

##### Option B

From this directory, run `./run-sv-comp.rb --pin --clean --reduced > svcomp-raw.txt 2> progress.txt`

**Expected Runtime**: Around 1h40min

If you want to keep track of the progress you can open an additional terminal `tail -f progress.txt`.

##### Option C

From this directory, run `./run-sv-comp.rb --pin --clean -j 40 > svcomp-raw.txt 2> progress.txt`

The `-j` parameter provides the number of concurrent runs to execute.

(The runtime here is on the magnitude of days with N=48 cores)

#### Step 2: Producing the plots

(For **Option A:** pass `--paper` as an additional argument to the scripts)

- For plot 2
    - Run `./plot2.sh` (Should this fail to the correct output check the individual steps listed in that file)
    - The resulting plot is located in `plots/2/plot2.pdf`
- For plot 3
    - Run `./plot3.sh` (Should this fail to the correct output check the individual steps listed in that file)
    - The resulting plot is located in `plots/3/plot3.pdf`

#### A list of claims from the paper supported by the artifact, and how/why

- Total number of tasks and terminating task
    - plots/2/timeout-counts.tex

- Fig. 2 (by reproducing the plot)
- Fig. 3 (by reproducing the plot)

- **RQ1**

    > Concerning (RQ1), we find a net precision improvement in about 52% of tasks when using the new update rule with the number of W/N switches limited to either 3 (ours3) or 20 (ours20). 13% (ours3) and 20% (ours20) of tasks show substantial improvements. Conversely, ours loses precision compared to the baseline on about 1% of tasks, where almost all show substantial losses in precision.

    - Run `./compare.py --compare ours3` and `./compare.py --compare ours20`
    - (For **Option A:** adding `--paper`)

    > The subcategory ReachSafety-Recursive benefits particularly from update rule ours (see fig. 3). Both analyses provide substantial improvements in about 43% of cases. Interestingly, the share of degraded cases is also greater with about 4%.

    - Run `./compare.py --compare ours3 --rec` and `./compare.py --compare ours20 --rec`
    - (For **Option A:** adding `--paper`)

- **RQ2**

    > W.r.t. (RQ2), the update rule apinis produces an improvement only in about 40% of tasks, which are substantial only in 7% of tasks. At the same time, 18% of the tasks have a net precision loss and 11% a substantial loss.

    - Run `./compare.py --compare apinis`
    - (For **Option A:** adding `--paper`)

    > On the subset of ReachSafety-Recursive the update rule apinis achieves substantial precision improvements only in about 21% of cases, whereas it is 43% for the new update rule.

    - Run `./compare.py --compare apinis --rec`
    - (For **Option A:** adding `--paper`)

- **RQ3**:

    > Regarding (RQ3), the update rule ours yields an improvement on 52% of tasks, regardless of whether the widening limit was set to 3 or 20. However, with a W/N limit of 20, 20% of tasks show substantial improvements, instead of 13% with the limit of 3. On ReachSafety-Recursive, with both limits to the number of W/N switches, an equal amount of precision is recovered (43%).

    - See **RQ1**.

#### Expected Output for Option B

By necessity, the results on this set do not fully coincide with the results on the full set. The subset for **B** was constructed to contain all the cases needed for _full_ reproduction of the recursive subset, and by then subsequently adding categories while making sure the overall runtime inside the artifact remains acceptable. Unfortunately, a subset showing all effects but still having a reasonable runtime remained elusive.

SV-COMP has large groups of benchmarks that share similar characteristics, and the choice of whether these are included into the subset or not makes a huge difference for the observed numbers on the subset.
A detailed investigation of what separates those tasks where effects can be observed from those where no or only small effects can be observed is certainly interesting. However, we think it is a topic for future work,
and deem it to be out of scope for the artifact evaluation.

Nevertheless, we expect that these experiments, together with the extraction from the logs (Option **A**) and the fact that the recursive set is fully reproduced, inspire confidence in our overall experimental setup and results without having to take on the burden of a full reproduction involving multiple days of runtime on highly parallel machines.

- Plots: See directory `plots/b_{2,3}`
- Total number of tasks and terminating task:
    - `plots/b_2/timeout-counts.tex`
    - The subset was chosen to not include any tasks where the 15min timeout is hit to avoid excessive runtimes

- Expected output for `./compare.py --compare ours3`

```
Total entries: 992
improved (> 0%) : 172 (17.34%)
improved significantly (>= 5%) : 162 (16.33%)
worsened (< 0%) : 55 (5.54%)
worsened significantly (<= -5%) : 42 (4.23%)
```

- Expected output for `./compare.py --compare ours20`

```
Total entries: 992
improved (> 0%) : 172 (17.34%)
improved significantly (>= 5%) : 162 (16.33%)
worsened (< 0%) : 55 (5.54%)
worsened significantly (<= -5%) : 42 (4.23%)
```

- Expected output for `./compare.py --compare ours3 --rec`

```
Total entries: 156
improved (> 0%) : 67 (42.95%)
improved significantly (>= 5%) : 66 (42.31%)
worsened (< 0%) : 8 (5.13%)
worsened significantly (<= -5%) : 8 (5.13%)
```

- Expected output for `./compare.py --compare ours20 --rec`

```
Total entries: 156
improved (> 0%) : 67 (42.95%)
improved significantly (>= 5%) : 66 (42.31%)
worsened (< 0%) : 8 (5.13%)
worsened significantly (<= -5%) : 8 (5.13%)
```

- Expected output for `./compare.py --compare apinis`

```
Total entries: 992
improved (> 0%) : 90 (9.07%)
improved significantly (>= 5%) : 71 (7.16%)
worsened (< 0%) : 293 (29.54%)
worsened significantly (<= -5%) : 291 (29.33%)
```

- Expected output for `./compare.py --compare apinis --rec`

```
Total entries: 156
improved (> 0%) : 37 (23.72%)
improved significantly (>= 5%) : 34 (21.79%)
worsened (< 0%) : 22 (14.10%)
worsened significantly (<= -5%) : 20 (12.82%)
```

To further understand why these numbers differ from the ones observed in the overall set, one may additionally run `./compare_per_cat.py --compare ours20 --paper` where `ours20` can be replaced with any of the other options.
One can observe that the impact of our update rules varies widely within these suites as provided by SV-COMP. Our hypothesis is that programs placed in the same category are roughly similar, though we have not yet investigated
how program properties relate to precision gains.

#### General setup of SV-COMP

- Clone sv-benchmarks at specific tag `svcomp24` into the parent directory `git clone --branch svcomp24-final --depth 1 https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks.git`

### Data for Experiments for RQ4

In this folder (`~/precision-recovery-mixed-flowsens-benchmarks/rq4-results`), we provide the resulting tables from runs on SV-COMP controlled under Benchexec.

#### A list of claims from the paper supported by the artifact, and how/why

> Overall, the analyses have almost identical runtimes on the tasks when they terminate. An exception is the configuration apinis, for which quite a few outliers with high runtime overheads are observed.

- Open `table-generator.table.html` and navigate to the tab titled `Scatter Plot`.
- Change `Correct only` for `Results:` to `All`.
- Change `X-Axis` to `cputime` for `base`.
- Change `Y-Axis` to `cputime` for one of `ours3`, `ours20`, or `apinis`.

The resulting plot shows runtimes of the `base` approach vs. the other. Each point is one program.
For points on the diagonal, both approaches took the same time. For points below it, the other approach was faster;
for those above it, `base` was faster.

> The analyses with WizWoz's default update rule (972) and the update rule ours with a limit of 3 (975) and 20 (974) to the number of W/N switches, fail to complete in a similar number of cases, whereas the analysis with the update rule extracted from Apinis et al. (1003) fails to complete slightly more often

- Open `table-generator.table.html` and navigate to the tab titled `Table`.
- Then in the column `Goblint [...] base` (last one), select `Category -> Error`.
- The number at the top right `Showing 972 of 11222 tasks` gives the number of failures.

- Repeat for `ours3`, `ours20`, and `apinis`.

> The majority of timeouts and other failures are shared between all considered analyses.

Open `table-generator.diff.html`. This lists all tasks where the type of failure differs or one of the analysis produced a timeout and the other did not. Observe that it is only `42/11222` tasks where this is the case.

#### A list of claims from the paper not supported by the artifact, and how/why

Our motivation for (RQ4) is producing reliable performance measurements by using benchexec which uses cgroups and other mechanisms to ensure that runtimes are reliable. Given the virtualization which occurs by using VirtualBox, the results of such a run would be no more representative than runs conducted with the setup we used for (RQ1-3). While the configurations for (RQ1-3) incur some overhead for marshaling, it is the same for all update rules.
Thus, the results from that run can stand in to inspire confidence in the numbers for (RQ4) but cannot directly reproduce them.

### Reproduction of Experiments for RQ5

The folder structure for this RQ (`~/precision-recovery-mixed-flowsens-benchmarks/rq5-benchmarks`) is as follows:

- `concrat`: The benchmark programs assembled by [Hong and Ryu 2023]
- `pthread`: The pthread benchmark programs from [Schwarz et al. 2021, 2023b]
- `svcomp`: The device driver benchmark programs from [Schwarz et al. 2021, 2023b]
- `paper-runs`: Folder containing the raw data of the runs underlying the plots in the paper
- `plots`: Tex files with pgplots plots for plotting results
- `bench.txt`: Specifies the full list of benchmark programs
- `reduced.txt`: Specifies the reduced list of benchmark programs recommended for reproduction
- Various Ruby / Python / Shell scripts

There are several levels of reproduction possible here:

- **A**: The folder `paper-runs` contains the data produced by the runs we used for the paper. This allows reproducing the plots and data from our raw results. **(Recommended)**
- **B**: We provide a subset of the benchmarks which can realistically be reproduced inside the VM **(Recommended)**
- **C**: We provide the scripts for a full reproduction. The runtime for this is several days. **(Not Recommended)**

#### Producing the raw data

The raw data here takes the form of two text files `plot4-7-raw.txt` and `plot5-6-raw.txt`. The folder `paper-runs` contains the version of this raw data corresponding to the run in the paper.

##### Option A

As our raw data is used, nothing is to be done.

##### Option B

1. Change into the analyzer repo, run `git checkout pldi25_eval_runtime` and `make release`
2. From the `rq5-benchmarks`, run `./update_bench_propertimings.rb --i 1 --reduced &> plot5-6-raw.txt`
    (By modifying the number supplied after `i`, you can specify that the average of more runs should be taken. To reduce runtimes, we recommend against it when reproducing results)

    **Expected Runtime**: Around 20min

3. Change into the analyzer repo, run `git checkout pldi25_eval_stats` and `make release`
4. From the `rq5-benchmarks`, run `./update_bench_fullpreccmp.rb --idead --priv-only --reduced &> plot4-7-raw.txt`

    **Expected Runtime**: Around 25min

**Note:** Those with more patience may add programs from `bench.txt` back to `reduced.txt` to increase the subset of considered programs.

##### Option C

1. Change into the analyzer repo, run `git checkout pldi25_eval_runtime` and `make release`
2. From the `rq5-benchmarks`, run `./update_bench_propertimings.rb --i 1 &> plot5-6-raw.txt`
    (By modifying the number supplied after `i`, you can specify that the average of more runs should be taken. To reduce runtimes, we recommend against it when reproducing results)

3. Change into the analyzer repo, run `git checkout pldi25_eval_stats` and `make release`
4. From the `rq5-benchmarks`, run `./update_bench_fullpreccmp.rb --idead --priv-only &> plot4-7-raw.txt`

#### Producing the plots

(For **Option A:** pass `--paper` as an additional argument to the scripts)

- Steps for reproducing plots `5 & 6`:
    - For plot 5
        - Run `./plot5.sh` (Should this fail to the correct output check the individual steps listed in that file)
        - The resulting plot is located in `plots/5/plot5.pdf`
    - For plot 6
        - Run `./plot6.sh` (Should this fail to the correct output check the individual steps listed in that file)
        - The resulting plot is located in `plots/6/plot6.pdf`

- Steps for reproducing plots `4 & 7`:
    - For plot 4:
        - Run `./plot4.sh` (Should this fail to the correct output check the individual steps listed in that file)
        - The resulting plot is located in `plots/4/plot4.pdf`
    - For plot 7:
        - Run `./plot7.sh` (Should this fail to the correct output check the individual steps listed in that file)
        - The resulting plot is located in `plots/7/plot7.pdf`

Depending on the runtimes observed in the VM, the `pgfplot` options may need to be modified a little, see the `*.tex` file in the folder `./plots/N`.

#### A list of claims from the paper supported by the artifact, and how/why

Plots 4-7: By reproducing them as outlined above

**N.B.**: Runtimes in the VM were observed to be quite a lot slower overall, though the relationship between configurations should still roughly be the same.

**Specific claims:**

- > When turning to net precision changes by aggregating precision losses and gains per program, we find that the update rule ours improves precision in 24 out of 38 cases, with 12 of these improvements being substantial. ours-bot improves precision for 32 programs, with 15 substantial improvements.

  > ours and ours-bot worsen net precision in only three and two cases, respectively, with two cases showing substantial losses for both approaches.

    - Run `./compare.py --compare ours` and `./compare.py --compare ours-bot`
    - (For **Option A:** adding `--paper`)

- > ours-bot always improves at least as many unknowns as ours and often more.

    - From Plot 4

- > The update rule apinis, on the other hand, improves net precision in only 11 cases, with six of these being substantial. Conversely, it worsens net precision in 18 cases, with 11 of these precision losses being substantial.

    - Run `./compare.py --compare apinis`
    - (For **Option A:** adding `--paper`)

- > Abstract garbage collection generally comes with a moderate slowdown, but causes a slowdown by a factor of 2.72 in the extreme.

    - From Plot 5 and Running `./overhead.py`
    - (For **Option A:** adding `--paper`)

- > For 18 out of 38 cases, over 40% of contexts are identified as dead.

    - Run `./dead-contexts.py`
    - (For **Option A:** adding `--paper`)

  > We find that ours-bot increases memory usage by 15% for two programs, while it yields a reduction for 18 programs. At the extremes, the magnitude of the reduction is considerable. For ypbind and smtprc, e.g., the heap memory footprint is roughly halved.

    - Run `./garbage.py`
    - (For **Option A:** adding `--paper`)

#### Expected Output for Option B

- Plots: See directory `plots/b_{4,5,6,7}`

- Expected output for `./compare.py --compare ours`

```
Total entries: 11
improved (> 0%) : 4 (36.36%)
improved significantly (>= 5%) : 1 (9.09%)
worsened (< 0%) : 1 (9.09%)
worsened significantly (<= -5%) : 0 (0.00%)
```

- Expected output for `./compare.py --compare ours-bot`

```
Total entries: 11
improved (> 0%) : 10 (90.91%)
improved significantly (>= 5%) : 3 (27.27%)
worsened (< 0%) : 0 (0.00%)
worsened significantly (<= -5%) : 0 (0.00%)
```

- Expected output for `./compare.py --compare apinis`

```
Total entries: 11
improved (> 0%) : 3 (27.27%)
improved significantly (>= 5%) : 2 (18.18%)
worsened (< 0%) : 2 (18.18%)
worsened significantly (<= -5%) : 1 (9.09%)
```

- Expected output for `./overhead.py`

```
Maximal overhead of 'ours-bot' over 'ours': 1.79
Corresponding row: {'suite': 'SV-COMP (ordered by logical LoC within this group)', 'file': 'marvell2', 'coordindex': '11', 'ours': '33.537565', 'ours-memory': '132.0', 'apinis': '39.773272', 'apinis-memory': '114.78', 'ours-bot': '60.106936', 'ours-bot-memory': '114.78', 'base': '39.964723', 'base-memory': '132.0'}
```

- Expected output for `./dead-contexts.py`

```
Total entries: 11
40% or more identified : 4
```

- Expected output for `./garbage.py`

```
Memory usage of ours-bot as fraction of ours for tegra20 : 0.76
Memory usage of ours-bot as fraction of ours for marvell1 : 0.76
Memory usage of ours-bot as fraction of ours for marvell2 : 0.87
Memory usage of ours-bot as fraction of ours for nsc : 0.76
Memory usage of ours-bot as fraction of ours for adutux : 1.00
Memory usage of ours-bot as fraction of ours for iowarrior : 0.87
Memory usage of ours-bot as fraction of ours for w83977af : 1.00
Memory usage of ours-bot as fraction of ours for ctrace : 1.00
Memory usage of ours-bot as fraction of ours for pfscan : 0.87
Memory usage of ours-bot as fraction of ours for knot : 1.00
Memory usage of ours-bot as fraction of ours for aget : 0.87
Gain: 3
Loss: 0
```

## Reusability

There are several axes along which other researchers can build on this work; we briefly sketch two of these here:

### Re-use the analyzer

The Goblint analyzer comes with extensive documentation (see `analyzer/docs`) or (perhaps more conveniently) the [rendered online version](https://goblint.readthedocs.io/). Of particular relevance here are:

- The step-to-step user guide and the instructions for running on larger projects (<https://goblint.readthedocs.io/en/latest/user-guide/running/#project-analysis>). In this way, it is possible to conduct experiments on further programs.
- The accessible step-by-step tutorial on adding custom analyses to the framework at <https://goblint.readthedocs.io/en/latest/developer-guide/firstanalysis/>. In the interest of a concise file, we opted against inlining this tutorial here.
- Any newly added analysis will be able to benefit from the update rules provided here.

Thus, the framework can serve as a testbed for new ideas for static analysis, allowing researchers to focus on the aspect of the system they are interested in while relying on the framework for everything else.
The changes made to Goblint to implement the update rules are outlined in the external repository's [CODE.md](https://github.com/tum-cit-pl/precision-recovery-mixed-flowsens-benchmarks/blob/main/CODE.md).

### Example

An example program `example.c` is provided in the top folder of `analyzer` to highlight precision differences between configurations.
For details see the external repository's [EXAMPLE.md](https://github.com/tum-cit-pl/precision-recovery-mixed-flowsens-benchmarks/blob/main/EXAMPLE.md).

### Experiment with further programs within this VM

All scripts are written in a way where it is easy to add further programs to the setup, or experiment with different settings and analyses.
To this end, it suffices to modify the respective `bench.txt` files by, e.g.,

- a) adding additional programs on which to experiment to the end or
- b) specifying additional configurations which to run at the beginning of the file.

## General setup of analyzer

- Clone analyzer into top-level directory `git clone git@github.com:goblint/analyzer.git`
- `cd analyzer`
- `git checkout pldi25_eval_stats` (`pldi25_eval_runtime` will do too, dependencies are identical)
- `make setup`
- `make dev`
- `make release`

## Acknowledgments

We would like to thank the anonymous reviewers for their valuable feedback, which was instrumental in polishing the paper.
DeepSeek was used to help with the creation of scripts for extracting precision data from logfiles.
This work was supported in part by Deutsche Forschungsgemeinschaft (DFG) -- 378803395/2428 ConVeY.
