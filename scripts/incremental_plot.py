import incremental_benchmark_utils as utils
import os
import pandas

def distribution_absdiff_plot(result_csv_filename):
    df=pandas.read_csv(result_csv_filename, index_col=0)
    # clean dataset (remove all rows for which any of the runtime entries is 0 which means that the respective analysis
    # run failed)
    df = df[(df[utils.header_runtime_parent] != 0) & (df[utils.header_runtime_incr_child] != 0) & (df[utils.header_runtime_incr_rel_child] != 0)]

    # plot incremental vs non-incremental
    diff = df.loc[:,utils.header_runtime_parent] - df.loc[:,utils.header_runtime_incr_child]
    utils.hist_plot(diff, 20, 'Improvement in s (incremental compared to non-incremental)', 'Number of Commits', "figure_absdiff_distr_incr.pdf")

    # plot reluctant vs. basic incremental
    diff = df.loc[:,utils.header_runtime_incr_child] - df.loc[:,utils.header_runtime_incr_rel_child]
    utils.hist_plot(diff, 2, 'Improvement in s (reluctant compared to incremental)', 'Number of Commits', "figure_absdiff_distr_rel.pdf", cutoff = {"bulk_max":50, "outliers_min":205, "outliers_max":230})

def distribution_reldiff_plot(result_csv_filename):
    df=pandas.read_csv(result_csv_filename, index_col=0)
    # clean dataset (remove all rows for which any of the runtime entries is 0 which means that the respective analysis
    # run failed)
    df = df[(df[utils.header_runtime_parent] != 0) & (df[utils.header_runtime_incr_child] != 0) & (df[utils.header_runtime_incr_rel_child] != 0)]

    # plot incremental vs non-incremental
    diff = df.loc[:,utils.header_runtime_incr_child] / df.loc[:,utils.header_runtime_parent]
    utils.hist_plot(diff, 0.01, 'Relative Improvement in s (incremental compared to non-incremental)', 'Number of Commits', "figure_reldiff_distr_incr.pdf")

    # plot reluctant vs. basic incremental
    diff = df.loc[:,utils.header_runtime_incr_rel_child] / df.loc[:,utils.header_runtime_incr_child]
    utils.hist_plot(diff, 0.02, 'Relative Improvement (reluctant compared to incremental)', 'Number of Commits', "figure_reldiff_distr_rel.pdf", cutoff = {"bulk_max":55, "outliers_min":135, "outliers_max":168})


dir = os.getcwd()
filename = "total_results.csv"
distribution_absdiff_plot(os.path.join(dir,filename))
distribution_reldiff_plot(os.path.join(dir,filename))
