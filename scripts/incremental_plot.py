import incremental_benchmark_utils as utils
import os
import pandas

def distribution_absdiff_plot(title, result_csv_filename, outdir, cutoffs_incr=None, cutoffs_rel=None):
    df=pandas.read_csv(result_csv_filename, index_col=0)
    # clean dataset (remove all rows for which any of the runtime entries is 0 which means that the respective analysis
    # run failed)
    df = df[df["Changed/Added/Removed functions"] > 0]
    df = df[(df[utils.header_runtime_parent] != 0) & (df[utils.header_runtime_incr_child] != 0) & (df[utils.header_runtime_incr_rel_child] != 0)]

    # plot incremental vs non-incremental
    diff = df.loc[:,utils.header_runtime_parent] - df.loc[:,utils.header_runtime_incr_child]
    utils.hist_plot(diff, 20, title, 'Improvement in s (incremental compared to non-incremental)', 'Number of Commits', os.path.join(outdir, "figure_absdiff_distr_incr.pdf"), cutoffs_incr)

    # plot reluctant vs. basic incremental
    diff = df.loc[:,utils.header_runtime_incr_child] - df.loc[:,utils.header_runtime_incr_rel_child]
    utils.hist_plot(diff, 2, title, 'Improvement in s (reluctant compared to incremental)', 'Number of Commits', os.path.join(outdir, "figure_absdiff_distr_rel.pdf"), cutoffs_rel)

def distribution_reldiff_plot(title, result_csv_filename, outdir, cutoffs_incr=None, cutoffs_rel=None):
    df=pandas.read_csv(result_csv_filename, index_col=0)
    # clean dataset (remove all rows for which any of the runtime entries is 0 which means that the respective analysis
    # run failed)
    df = df[df["Changed/Added/Removed functions"] > 0]
    df = df[(df[utils.header_runtime_parent] != 0) & (df[utils.header_runtime_incr_child] != 0) & (df[utils.header_runtime_incr_rel_child] != 0)]

    # plot incremental vs non-incremental
    diff = 1 - df.loc[:,utils.header_runtime_incr_child] / df.loc[:,utils.header_runtime_parent]
    utils.hist_plot(diff, 0.01, title, "Relative Improvement in s (incremental compared to non-incremental)", 'Number of Commits', os.path.join(outdir, "figure_reldiff_distr_incr.pdf"), cutoffs_incr)

    # plot reluctant vs. basic incremental
    diff = 1 - df.loc[:,utils.header_runtime_incr_rel_child] / df.loc[:,utils.header_runtime_incr_child]
    utils.hist_plot(diff, 0.005, title, 'Relative Improvement (reluctant compared to incremental)', 'Number of Commits', os.path.join(outdir, "figure_reldiff_distr_rel.pdf"), cutoffs_rel)



# zstd with postsolver
# dir_with = "zstd-9-line0"
# filename_with = "total_results.csv"
# filepath_with = os.path.join(dir_with,filename_with)
# title = "With the incremental postsolver"
# distribution_absdiff_plot(title, filepath_with, dir_with) #cutoffs_rel={"bulk_max":50, "outliers_min":205, "outliers_max":230}
# distribution_reldiff_plot(title, filepath_with, dir_with)

# zstd without postsolver
dir_without = "zstd-13-balanced-20-noincrpostsolver"
filename_without = "total_results.csv"
filepath_without = os.path.join(dir_without,filename_without)
title = "Without the incremental postsolver"
distribution_absdiff_plot(title, filepath_without, dir_without)
distribution_reldiff_plot(title, filepath_without, dir_without) # cutoffs_rel=((0,60),(195,205),(235,245)))
