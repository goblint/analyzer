import utils
import os

def cummulative_distr_compare2(result_csv_filename):
    num_bins = 2000
    outfile_nonincr_vs_incr = "figure_cum_distr_incr.pdf"
    outfile_incr_vs_incrrel = "figure_cum_distr_rel.pdf"
    df = utils.get_cleaned_filtered_data(result_csv_filename, filterDetectedChanges=True)

    data, base = utils.create_cum_data(df, num_bins, [utils.header_runtime_parent, utils.header_runtime_incr_child])
    datanonincr = {"values": data[0], "label": "Non-incremental analysis of parent commit"}
    dataincr = {"values": data[1], "label": "Incremental analysis of commit"}
    utils.cummulative_distr_plot([datanonincr, dataincr], base, outfile_nonincr_vs_incr)

    data, base = utils.create_cum_data(df, num_bins, [utils.header_runtime_incr_child, utils.header_runtime_incr_rel_child])
    dataincr = {"values": data[0], "label": "Incremental analysis of commit"}
    datarelincr = {"values": data[1], "label": "Reluctant incremental analysis of commit"}
    utils.cummulative_distr_plot([dataincr, datarelincr], base, outfile_incr_vs_incrrel, logscale=True)

def cummulative_distr_all3(result_csv_filename):
    num_bins = 2000
    outfile_nonincr_vs_incr = "figure_cum_distr_all3.pdf"
    df = utils.get_cleaned_filtered_data(result_csv_filename, filterDetectedChanges=True)

    data, base = utils.create_cum_data(df, num_bins, [utils.header_runtime_parent, utils.header_runtime_incr_child, utils.header_runtime_incr_rel_child])
    datanonincr = {"values": data[0], "label": "Non-incremental analysis of parent commit"}
    dataincr = {"values": data[1], "label": "Incremental analysis of commit"}
    datarelincr = {"values": data[2], "label": "Reluctant incremental analysis of commit"}
    utils.cummulative_distr_plot([datanonincr, dataincr, datarelincr], base, outfile_nonincr_vs_incr, figsize=(6,4), logscale=True)

def distribution_absdiff_plot(title, result_csv_filename, outdir, cutoffs_incr=None, cutoffs_rel=None):
    df = utils.get_cleaned_filtered_data(result_csv_filename, filterDetectedChanges=True)

    # plot incremental vs non-incremental
    diff = df.loc[:,utils.header_runtime_parent] - df.loc[:,utils.header_runtime_incr_child]
    utils.hist_plot(diff, 20, title, 'Improvement in s (incremental compared to non-incremental)', 'Number of Commits', os.path.join(outdir, "figure_absdiff_distr_incr.pdf"), cutoffs_incr)

    # plot reluctant vs. basic incremental
    diff = df.loc[:,utils.header_runtime_incr_child] - df.loc[:,utils.header_runtime_incr_rel_child]
    utils.hist_plot(diff, 2, title, 'Improvement in s (reluctant compared to incremental)', 'Number of Commits', os.path.join(outdir, "figure_absdiff_distr_rel.pdf"), cutoffs_rel)

def distribution_reldiff_plot(title, result_csv_filename, outdir, cutoffs_incr=None, cutoffs_rel=None):
    df = utils.get_cleaned_filtered_data(result_csv_filename, filterDetectedChanges=True)

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
cummulative_distr_all3(filepath_without)
