import os
import shutil
from pathlib import Path
import subprocess
from pydriller import Git
import re
import pandas
import json
import numpy as np
import brokenaxes
import matplotlib as mpl
mpl.use("pgf")
mpl.rcParams.update({
    "pgf.texsystem": "pdflatex",
    'pgf.rcfonts': False,
    'text.usetex': True,
    'font.family': 'serif',
    'font.size': 9,
    'axes.titlesize': 9,
    'legend.fontsize': 9,
    'figure.titlesize': 9,
    'figure.dpi': 300,
    'xtick.labelsize': 9,
    'ytick.labelsize': 9,

})
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter

header_runtime_parent = "Runtime for parent commit (non-incremental)"
header_runtime_incr_child = "Runtime for commit (incremental)"
header_runtime_incr_posts_child = "Runtime for commit (incremental + incr postsolver)"
header_runtime_incr_posts_rel_child = "Runtime for commit (incremental + incr postsolver + reluctant)"

preparelog = "prepare.log"
analyzerlog = "analyzer.log"
comparelog = "compare.log"

def reset_incremental_data(incr_data_dir):
    if os.path.exists(incr_data_dir) and os.path.isdir(incr_data_dir):
        shutil.rmtree(incr_data_dir)

def analyze_commit(analyzer_dir, gr : Git, repo_path, build_compdb, commit_hash, outdir, conf, extra_options):
    gr.checkout(commit_hash)
    conf_path = os.path.join(analyzer_dir, 'conf', conf + '.json')

    # print configuration
    with open(outdir+'/config.out', "a+") as file:
        with open(conf_path, "r") as c:
            file.write("config: " + c.read())
            file.write("\n")
            file.write("added options:\n")
            for o in extra_options:
                file.write(o + " ")
            file.close()

    prepare_command = ['sh', os.path.join(analyzer_dir, 'scripts', build_compdb)]
    with open(os.path.join(outdir, preparelog), "w+") as outfile:
        subprocess.run(prepare_command, cwd = gr.path, check=True, stdout=outfile, stderr=subprocess.STDOUT)
        outfile.close()

    analyze_command = [os.path.join(analyzer_dir, 'goblint'), '--conf', os.path.join(analyzer_dir, 'conf', conf + '.json'), *extra_options, repo_path]
    with open(os.path.join(outdir, analyzerlog), "w+") as outfile:
        subprocess.run(analyze_command, check=True, stdout=outfile, stderr=subprocess.STDOUT)
        outfile.close()

def compare_runs(analyzer_dir, dummy_c_file, outdir, conf, compare_data_1, compare_data_2):
    options = ['--conf', os.path.join(analyzer_dir, 'conf', conf + '.json'), '--disable', 'printstats', '--disable', 'warn.warning', '--disable', 'warn.race', '--disable', 'dbg.compare_runs.diff', '--disable', 'dbg.compare_runs.eqsys', '--enable', 'dbg.compare_runs.node', '--compare_runs', compare_data_1, compare_data_2]
    analyze_command = [os.path.join(analyzer_dir, 'goblint'), *options, dummy_c_file]
    with open(os.path.join(outdir, comparelog), "w+") as outfile:
        subprocess.run(analyze_command, check=True, stdout=outfile, stderr=subprocess.STDOUT)
        outfile.close()

def calculateRelCLOC(repo_path, commit, diff_exclude):
    diff_exclude = list(map(lambda x: os.path.join(repo_path, x), diff_exclude))
    relcloc = 0
    for f in commit.modified_files:
        _, extension = os.path.splitext(f.filename)
        if not (extension == ".h" or extension == ".c"):
            continue
        filepath = f.new_path
        if filepath is None:
            filepath = f.old_path
        parents = Path(filepath).parents
        parents = list(map(lambda x: os.path.join(repo_path, x), parents))
        if any(dir in parents for dir in diff_exclude):
            continue
        relcloc = relcloc + f.added_lines + f.deleted_lines
    return relcloc

def find_line(pattern, log):
    with open (log, 'r') as file:
        for line in file:
            m = re.search(pattern, line)
            if m:
                file.close()
                return m.groupdict()
        return None

def extract_from_analyzer_log(log):
    runtime_pattern = 'TOTAL[ ]+(?P<runtime>[0-9\.]+) s'
    change_info_pattern = 'change_info = { unchanged = (?P<unchanged>[0-9]*); changed = (?P<changed>[0-9]*); added = (?P<added>[0-9]*); removed = (?P<removed>[0-9]*) }'
    r = find_line(runtime_pattern, log)
    ch = find_line(change_info_pattern, log) or {"unchanged": 0, "changed": 0, "added": 0, "removed": 0}
    d = dict(list(r.items()) + list(ch.items()))
    with open(log, "r") as file:
        num_racewarnings = file.read().count('[Warning][Race]')
        d["race_warnings"] = num_racewarnings
        file.close()
    return d

def extract_precision_from_compare_log(log):
    pattern = "equal: (?P<equal>[0-9]+), more precise: (?P<moreprec>[0-9]+), less precise: (?P<lessprec>[0-9]+), incomparable: (?P<incomp>[0-9]+), total: (?P<total>[0-9]+)"
    precision = find_line(pattern, log)
    return {k: int(v) for k,v in precision.items()} if precision else None

def barplot(data_set):
    df = pandas.DataFrame(data_set["data"], index=data_set["index"]) # TODO: index=analyzed_commits
    df.sort_index(inplace=True, key=lambda idx: idx.map(lambda x: int(x.split(":")[0])))
    print(df)
    df.to_csv('results.csv')

    df.plot.bar(rot=0, width=0.7, figsize=(25,10))
    plt.xticks(rotation=45, ha='right', rotation_mode='anchor')
    plt.xlabel('Commit')
    plt.tight_layout()
    plt.savefig("figure.pdf")

def get_cleaned_filtered_data(result_csv_file, filterRelCLOC=False, filterDetectedChanges=False):
    df=pandas.read_csv(result_csv_file, index_col='Commit', sep=";")
    df = df.loc[:, ~df.columns.str.contains('^Unnamed')]

    # clean dataset (remove all rows for which any of the runtime entries is 0 which means that the respective analysis
    # run failed)
    df = df[(df[header_runtime_parent] != 0)]
    if filterRelCLOC:
        df = df[df["Relevant changed LOC"] > 0]
    if filterDetectedChanges:
        df = df[df["Changed/Added/Removed functions"] > 0]
    return df

def get_data_from_json(result_file):
    with open(result_file) as f:
        d = json.load(f)
    df=pandas.json_normalize(d['seq_summary'])
    return df

def create_cum_data(dataFrame, num_bins, relColumns):
    min = dataFrame[relColumns].min().min()
    max = dataFrame[relColumns].max().max()
    bins = np.linspace(min,max,num=num_bins+1)
    data = []
    base = []
    for c in relColumns:
        valuesc, basec = np.histogram(dataFrame.loc[:,c], bins=bins)
        base = basec
        cum = np.cumsum(valuesc, dtype=np.float)
        cum[cum==0] = np.nan
        data = data + [cum]
    return data, base[:-1]

def cummulative_distr_plot(data_sets, base, outfile, figsize=None, title=None, logscale=False):
    if figsize:
        plt.figure(figsize=figsize)
    else:
        plt.figure()
    for d in data_sets:
        plt.plot(d["values"], base, label=d["label"])
    plt.xlabel('Number of Commits')
    if logscale:
        plt.ylabel('Runtime in s ($log_{2}$ scale)')
        plt.yscale('log', base=2)
        plt.gca().yaxis.set_major_formatter(ScalarFormatter())
        plt.xlim(left=0)
        plt.ylim(bottom=95)
        #plt.yticks(np.arange(100,1500,100))
    else:
        plt.ylabel('Runtime in s')
    plt.tight_layout()
    plt.legend()
    plt.title(title)
    plt.savefig(outfile)

def hist_plot(data, step, title, xlabel, ylabel, outfile, size, xlim_left=None, xlim_right=None, cutoffs=None):
    min = data.min()
    max = data.max()
    min = min//step
    max = max//step + 1
    bins = np.arange(min*step,(max+1)*step,step)

    if cutoffs:
        plt.figure()
        bax = brokenaxes.brokenaxes(ylims=cutoffs, hspace=0.05, left = 0.18, bottom = 0.16)
        bax.hist(data, bins, histtype='bar')
        plt.xlabel(xlabel, labelpad=0)
        plt.ylabel(ylabel, labelpad=0)
        if title: plt.title(title)
        plt.savefig(outfile, bbox_inches='tight')
    else:
        fig = plt.figure()
        width, height = size
        fig.set_size_inches(w=width, h=height)
        plt.hist(data, bins)
        if xlim_left:
            plt.xlim(left=xlim_left, right=xlim_right)
        else:
            plt.xlim(right=xlim_right)
        if xlabel: plt.xlabel(xlabel)
        if ylabel: plt.ylabel(ylabel)
        if title: plt.title(title)
        plt.tight_layout(pad=0.4)
        plt.savefig(outfile)

def hist_subplots(ax, data, step):
    min = data.min()
    max = data.max()
    min = min//step
    max = max//step + 1
    bins = np.arange(min*step,(max+1)*step,step)
    ax.hist(data, bins)

def four_hist_subplots(data, title, xlabel, ylabel, outfile):
    step = 0.01
    fig, ((ax1,ax2),(ax3,ax4)) = plt.subplots(2,2,tight_layout=True)
    for i, ax in enumerate([ax1,ax2,ax3,ax4]):
        hist_subplots(ax, data, step)
        ax.title.set_text(title[i])
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.tight_layout()
    fig.savefig(outfile)

def scatter_plot(data, xlabel, ylabel, outfile, size):
    fig = plt.figure()
    width, height = size
    fig.set_size_inches(w=width, h=height)
    colors=['red','azure','blue','brown','chartreuse','chocolate','darkblue','darkgreen','seagreen','green','indigo','orangered','orange','coral','olive','mediumseagreen','grey','teal']
    markers = ['x','+','o','s','p','*','D','d','v','^','<','>','1','2','3','4','H','P']
    linestyles = ['dashed']
    for i, (x, y) in enumerate(data):
        plt.plot(x,y, marker='x', linewidth=0.4, markersize=1, alpha=0.85, color=colors[i % len(colors)], linestyle=linestyles[i % len(linestyles)])
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.ylim(bottom=-0.005, top=0.19)
    plt.tight_layout(pad=0.4)
    plt.savefig(outfile)
