import utils
import os

def cummulative_distr_compare2(outdir, result_csv_filename):
    num_bins = 2000
    outfile_nonincr_vs_incr = "figure_cum_distr_incr.pdf"
    outfile_incr_vs_incrrel = "figure_cum_distr_rel.pdf"
    df = utils.get_cleaned_filtered_data(os.path.join(outdir,result_csv_filename), filterDetectedChanges=True)

    data, base = utils.create_cum_data(df, num_bins, [utils.header_runtime_parent, utils.header_runtime_incr_child])
    datanonincr = {"values": data[0], "label": "Non-incremental analysis of parent commit"}
    dataincr = {"values": data[1], "label": "Incremental analysis of commit"}
    utils.cummulative_distr_plot([datanonincr, dataincr], base, outfile_nonincr_vs_incr)

    data, base = utils.create_cum_data(df, num_bins, [utils.header_runtime_incr_child, utils.header_runtime_incr_rel_child])
    dataincr = {"values": data[0], "label": "Incremental analysis of commit"}
    datarelincr = {"values": data[1], "label": "Reluctant incremental analysis of commit"}
    utils.cummulative_distr_plot([dataincr, datarelincr], base, outfile_incr_vs_incrrel, logscale=True)

def cummulative_distr_all3(outdir, result_csv_filename):
    num_bins = 2000
    outfile_nonincr_vs_incr = "figure_cum_distr_all3.pdf"
    df = utils.get_cleaned_filtered_data(os.path.join(outdir,result_csv_filename), filterDetectedChanges=True)

    data, base = utils.create_cum_data(df, num_bins, [utils.header_runtime_parent, utils.header_runtime_incr_child, utils.header_runtime_incr_rel_child])
    datanonincr = {"values": data[0], "label": "Non-incremental analysis of parent commit"}
    dataincr = {"values": data[1], "label": "Incremental analysis of commit"}
    datarelincr = {"values": data[2], "label": "Reluctant incremental analysis of commit"}
    utils.cummulative_distr_plot([datanonincr, dataincr, datarelincr], base, outfile_nonincr_vs_incr, figsize=(6,4), logscale=True)

def distribution_absdiff_plot(title, result_csv_filename, outdir, cutoffs_incr=None, cutoffs_rel=None):
    df = utils.get_cleaned_filtered_data(os.path.join(outdir,result_csv_filename), filterDetectedChanges=True)

    # plot incremental vs non-incremental
    diff = df.loc[:,utils.header_runtime_parent] - df.loc[:,utils.header_runtime_incr_child]
    utils.hist_plot(diff, 20, title, 'Improvement in s (incremental compared to non-incremental)', 'Number of Commits', os.path.join(outdir, "figure_absdiff_distr_incr.pdf"), cutoffs_incr)

    # plot reluctant vs. basic incremental
    diff = df.loc[:,utils.header_runtime_incr_child] - df.loc[:,utils.header_runtime_incr_rel_child]
    utils.hist_plot(diff, 2, title, 'Improvement in s (reluctant compared to incremental)', 'Number of Commits', os.path.join(outdir, "figure_absdiff_distr_rel.pdf"), cutoffs_rel)

def distribution_reldiff_plot(title, result_csv_filename, outdir, cutoffs_incr=None, cutoffs_rel=None):
    df = utils.get_cleaned_filtered_data(os.path.join(outdir,result_csv_filename), filterDetectedChanges=True)

    # plot incremental vs non-incremental
    print(df[utils.header_runtime_incr_child].astype('float'))
    diff = 1 - df[utils.header_runtime_incr_child].astype('float') / df[utils.header_runtime_parent].astype('float')
    utils.hist_plot(diff, 0.01, title, "Relative Improvement in s (incremental compared to non-incremental)", 'Number of Commits', os.path.join(outdir, "figure_reldiff_distr_incr.pdf"), cutoffs_incr)

    # plot reluctant vs. basic incremental
    diff = 1 - df.loc[:,utils.header_runtime_incr_rel_child] / df.loc[:,utils.header_runtime_incr_child]
    utils.hist_plot(diff, 0.005, title, 'Relative Improvement (reluctant compared to incremental)', 'Number of Commits', os.path.join(outdir, "figure_reldiff_distr_rel.pdf"), cutoffs_rel)

def paper_efficiency_graphs(dir_results_baseline, dir_results_incrps, csv_filename, outdir, filterRelCLOC=False, filterDetectedChanges=False):
    df_base = utils.get_cleaned_filtered_data(os.path.join(dir_results_baseline,csv_filename), filterRelCLOC=filterRelCLOC, filterDetectedChanges=filterDetectedChanges)
    df_incrps = utils.get_cleaned_filtered_data(os.path.join(dir_results_incrps,csv_filename), filterRelCLOC=filterRelCLOC, filterDetectedChanges=filterDetectedChanges)
    title1 = "Plain incremental\nvs. non-incr solver"
    title2 = "Plain incremental with incr. postsolver\nvs. Plain incremental"
    title3 = "Reluctant incremental with incr. postsolver\nvs. Plain incremental with incr. postsolver"
    title4 = "Reluctant incremental with incr. postsolver vs. non-incr solver"
    diff1 = 1 - df_base[utils.header_runtime_incr_child].astype('float') / df_base[utils.header_runtime_parent].astype('float')
    diff2 = 1 - df_incrps[utils.header_runtime_incr_child].astype('float') / df_base[utils.header_runtime_incr_child].astype('float')
    diff3 = 1 - df_incrps[utils.header_runtime_incr_rel_child].astype('float') / df_incrps[utils.header_runtime_incr_child].astype('float')
    diff4 = 1 - df_incrps[utils.header_runtime_incr_child].astype('float') / df_base[utils.header_runtime_parent].astype('float')
    step = 0.01
    for i, (diff,title) in enumerate([(diff1,title1),(diff2,title2),(diff3,title3),(diff4,title4)]):
        # output textwidth in latex with
        # \usepackage{layouts}
        # \printinunitsof{cm}\prntlen{\textwidth}
        # \printinunitsof{in}\prntlen{\textwidth}
        # -> 17.7917cm / 7.00697in
        if i == 3:
            size = (7, 7/4)
            # with title: size = (7, 7/3) # 3.54in = 9cm # use 18*cm for specifying it in cm
        else:
            size = (7/3, 7/4)
            # with title: size = (7/3.33, 7/3.33)
        utils.hist_plot(diff, step, None, "Relative speedup", "\#Commits", os.path.join(outdir, "efficiency_figure_" + str(i) + ".pgf"), size, xlim_right=1, cutoffs=None)


results_efficiency_baseline = "result_efficiency_incrpost" # TODO
results_efficiency_incrpostsolver = "result_efficiency_incrpost"
outdir = "figures"
os.mkdir(outdir)
filename = "total_results.csv"
# TODO discuss filtering (for reluctant the more the better)
paper_efficiency_graphs(results_efficiency_baseline, results_efficiency_incrpostsolver, filename, outdir, filterRelCLOC=True, filterDetectedChanges=False)
