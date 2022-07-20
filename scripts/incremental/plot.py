import utils
import os
import shutil

def cummulative_distr_compare2(outdir, result_csv_filename):
    num_bins = 2000
    outfile_nonincr_vs_incr = "figure_cum_distr_incr.pdf"
    outfile_incr_vs_incrrel = "figure_cum_distr_rel.pdf"
    df = utils.get_cleaned_filtered_data(os.path.join(outdir,result_csv_filename), filterDetectedChanges=True)

    data, base = utils.create_cum_data(df, num_bins, [utils.header_runtime_parent, utils.header_runtime_incr_child])
    datanonincr = {"values": data[0], "label": "Non-incremental analysis of parent commit"}
    dataincr = {"values": data[1], "label": "Incremental analysis of commit"}
    utils.cummulative_distr_plot([datanonincr, dataincr], base, outfile_nonincr_vs_incr)

    data, base = utils.create_cum_data(df, num_bins, [utils.header_runtime_incr_child, utils.header_runtime_incr_posts_rel_child])
    dataincr = {"values": data[0], "label": "Incremental analysis of commit"}
    datarelincr = {"values": data[1], "label": "Reluctant incremental analysis of commit"}
    utils.cummulative_distr_plot([dataincr, datarelincr], base, outfile_incr_vs_incrrel, logscale=True)

def cummulative_distr_all3(outdir, result_csv_filename):
    num_bins = 2000
    outfile_nonincr_vs_incr = "figure_cum_distr_all3.pdf"
    df = utils.get_cleaned_filtered_data(os.path.join(outdir,result_csv_filename), filterDetectedChanges=True)

    data, base = utils.create_cum_data(df, num_bins, [utils.header_runtime_parent, utils.header_runtime_incr_child, utils.header_runtime_incr_posts_rel_child])
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
    diff = df.loc[:,utils.header_runtime_incr_child] - df.loc[:,utils.header_runtime_incr_posts_rel_child]
    utils.hist_plot(diff, 2, title, 'Improvement in s (reluctant compared to incremental)', 'Number of Commits', os.path.join(outdir, "figure_absdiff_distr_rel.pdf"), cutoffs_rel)

def distribution_reldiff_plot(title, result_csv_filename, outdir, cutoffs_incr=None, cutoffs_rel=None):
    df = utils.get_cleaned_filtered_data(os.path.join(outdir,result_csv_filename), filterDetectedChanges=True)

    # plot incremental vs non-incremental
    print(df[utils.header_runtime_incr_child].astype('float'))
    diff = 1 - df[utils.header_runtime_incr_child].astype('float') / df[utils.header_runtime_parent].astype('float')
    utils.hist_plot(diff, 0.01, title, "Relative Improvement in s (incremental compared to non-incremental)", 'Number of Commits', os.path.join(outdir, "figure_reldiff_distr_incr.pdf"), cutoffs_incr)

    # plot reluctant vs. basic incremental
    diff = 1 - df.loc[:,utils.header_runtime_incr_posts_rel_child] / df.loc[:,utils.header_runtime_incr_child]
    utils.hist_plot(diff, 0.005, title, 'Relative Improvement (reluctant compared to incremental)', 'Number of Commits', os.path.join(outdir, "figure_reldiff_distr_rel.pdf"), cutoffs_rel)

def paper_efficiency_graphs(dir_results, csv_filename, outdir, filterRelCLOC=False, filterDetectedChanges=False):
    df = utils.get_cleaned_filtered_data(os.path.join(dir_results,csv_filename), filterRelCLOC=filterRelCLOC, filterDetectedChanges=filterDetectedChanges)
    diff1 = 1 - df[utils.header_runtime_incr_child].astype('float') / df[utils.header_runtime_parent].astype('float')
    diff2 = 1 - df[utils.header_runtime_incr_posts_child].astype('float') / df[utils.header_runtime_incr_child].astype('float')
    diff3 = 1 - df[utils.header_runtime_incr_posts_rel_child].astype('float') / df[utils.header_runtime_incr_posts_child].astype('float')
    diff4 = 1 - df[utils.header_runtime_incr_posts_rel_child].astype('float') / df[utils.header_runtime_parent].astype('float')
    step = 0.01
    for i, diff in enumerate([diff1,diff2,diff3,diff4]):
        # output textwidth in latex with
        # \usepackage{layouts}
        # \printinunitsof{cm}\prntlen{\textwidth}
        # \printinunitsof{in}\prntlen{\textwidth}
        # -> 17.7917cm / 7.00697in
        textwidth = 7
        xlimleft = None
        xlimright = 1.05
        xlabel = "Relative speedup" if i==3 else None
        ylabel = "\# Commits" if i==0 or i==3 else None
        outfile = os.path.join(outdir, "efficiency_figure_" + str(i) + ".pgf")
        if i == 0:
            size = (textwidth/3+0.1, textwidth/4) # additional ylabel
        elif i == 1:
            xlimleft = -0.3
            size = (textwidth/3-0.1/2, textwidth/4) # missing ylabel
        elif i == 3:
            size = (textwidth, textwidth/4)
            xlimright = 1.02
            step = 0.005
        else:
            size = (textwidth/3-0.1/2, textwidth/4) # missing ylabel
        utils.hist_plot(diff, step, None, xlabel, ylabel, outfile,
            size, xlim_left=xlimleft, xlim_right=xlimright, cutoffs=None)

        # print statistics
        for e in diff:
            if (xlimleft and e < xlimleft) or (xlimright and e > xlimright):
                print("excluded", e, "from efficiency figure", i)
    diff1 = df[utils.header_runtime_incr_child].astype('float') / df[utils.header_runtime_parent].astype('float')
    diff2 = df[utils.header_runtime_incr_posts_child].astype('float') / df[utils.header_runtime_parent].astype('float')
    diff3 = df[utils.header_runtime_incr_posts_rel_child].astype('float') / df[utils.header_runtime_parent].astype('float')
    for n, diff in [("incr", diff1), ("+ incr postsolver", diff2), ("+ reluctant", diff3)]:
        print("80% quantile for", n, "compared to from-scratch analysis:", diff.quantile(q=0.8) * 100, "%")
        print("75% quantile for", n, "compared to from-scratch analysis:", diff.quantile(q=0.75) * 100, "%")


def paper_precision_graph(results_precision, filename, outdir):
    df = utils.get_data_from_json(os.path.join(results_precision, filename))

    # Plot precision loss after x commits, where x is in {1, 2, 5, 10, 15}
    lessprec1 = 'intermediate precision.1.precision.lessprec'
    lessprec2 = 'intermediate precision.2.precision.lessprec'
    lessprec5 = 'intermediate precision.5.precision.lessprec'
    lessprec10 = 'intermediate precision.10.precision.lessprec'
    lessprec15 = 'intermediate precision.15.precision.lessprec'
    lessprecfinal = 'final precision.lessprec'
    total1 = 'intermediate precision.1.precision.total'
    total2 = 'intermediate precision.2.precision.total'
    total5 = 'intermediate precision.5.precision.total'
    total10 = 'intermediate precision.10.precision.total'
    total15 = 'intermediate precision.15.precision.total'
    totalfinal = 'final precision.total'

    data = []
    for i in range(len(df.index)):
        x = [1,2,5,10,15,df.iloc[i]['length']]
        vals = df.iloc[i][[lessprec1, lessprec2, lessprec5, lessprec10, lessprec15, lessprecfinal]].values
        total = df.iloc[i][[total1, total2, total5, total10, total15, totalfinal]].values
        x = [x[i] for i in range(len(x)) if vals[i] == vals[i]]
        y = [vals[i] / total[i] for i in range(len(vals)) if vals[i] == vals[i] and total[i] == total[i]]
        data.append((x,y))
    halftextwidth = 3.3
    size=(halftextwidth,halftextwidth*2/3)
    utils.scatter_plot(data, "\# Commits", "Share of less precise program points", os.path.join(outdir, "precision_figure.pgf"), size)



# efficiency plots
results_efficiency = "result_efficiency"
outdir = "figures"
if os.path.exists(outdir):
    shutil.rmtree(outdir)
os.mkdir(outdir)
filename = "total_results.csv"
paper_efficiency_graphs(results_efficiency, filename, outdir, filterRelCLOC=True, filterDetectedChanges=False)


# precision plot
results_precision = "result_precision"
filename = "results.json"
paper_precision_graph(results_precision, filename, outdir)
