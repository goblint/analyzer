import os
import shutil
from pathlib import Path
import subprocess
from pydriller import Git
import re
import pandas
import numpy as np
import matplotlib
matplotlib.use("pgf")
matplotlib.rcParams.update(
    {
        "pgf.texsystem": "pdflatex",
        "font.family": "serif",
        "font.size": 8,
        "text.usetex": True,
        "pgf.rcfonts": False,
        "axes.unicode_minus": False,
    }
)
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter

header_runtime_parent = "Runtime for parent commit (non-incremental)"
header_runtime_incr_child = "Runtime for commit (incremental)"
header_runtime_incr_rel_child = "Runtime for commit (incremental, reluctant)"

def reset_incremental_data(analyzer_dir):
    incr_data_dir = os.path.join(analyzer_dir, 'incremental_data')
    if os.path.exists(incr_data_dir) and os.path.isdir(incr_data_dir):
        shutil.rmtree(incr_data_dir)

def analyze_commit(analyzer_dir, gr : Git, repo_path, build_compdb, commit_hash, outdir, conf, extra_options):
    gr.checkout(commit_hash)

    prepare_command = ['sh', os.path.join(analyzer_dir, 'scripts', build_compdb)]
    with open(outdir+'/prepare.log', "w+") as outfile:
        subprocess.run(prepare_command, cwd = gr.path, check=True, stdout=outfile, stderr=subprocess.STDOUT)
        outfile.close()

    analyze_command = [os.path.join(analyzer_dir, 'goblint'), '--conf', os.path.join(analyzer_dir, 'conf', conf + '.json'), *extra_options, repo_path]
    with open(outdir+'/analyzer.log', "w+") as outfile:
        subprocess.run(analyze_command, check=True, stdout=outfile, stderr=subprocess.STDOUT)
        outfile.close()

def runPrivPrecCompare(filepath1, filepath2, analyzer_dir, outdir):
    compare_command = [os.path.join(analyzer_dir, 'privPrecCompare'), filepath1, filepath2]
    with open(outdir+'/privprec.log', "w+") as outfile:
        subprocess.run(compare_command, check=True, stdout=outfile, stderr=subprocess.STDOUT)
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

def extract_from_analyzer_log(log):
    def find_line(pattern):
        file = open(log, "r")
        for line in file.readlines():
            m = re.search(pattern, line)
            if m:
                file.close()
                return m.groupdict()
    runtime_pattern = 'TOTAL[ ]+(?P<runtime>[0-9\.]+) s'
    change_info_pattern = 'change_info = { unchanged = (?P<unchanged>[0-9]*); changed = (?P<changed>[0-9]*); added = (?P<added>[0-9]*); removed = (?P<removed>[0-9]*) }'
    r = find_line(runtime_pattern)
    ch = find_line(change_info_pattern) or {"unchanged": 0, "changed": 0, "added": 0, "removed": 0}
    d = dict(list(r.items()) + list(ch.items()))
    file = open(log, "r")
    num_racewarnings = file.read().count('[Warning][Race]')
    d["race_warnings"] = num_racewarnings
    file.close()
    return d

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

def cummulative_distr_plot(result_csv_filename):
    df=pandas.read_csv(result_csv_filename, index_col=0)
    # clean dataset (remove all rows for which any of the runtime entries is 0 which means that the respective analysis
    # run failed)
    df = df[(df[header_runtime_parent] != 0) & (df[header_runtime_incr_child] != 0) & (df[header_runtime_incr_rel_child] != 0)]

    #df = df[df["Relevant changed LOC"] > 0]
    #df = df[df["Changed/Added/Removed functions"] > 0]
    num_bins = 2000
    outfile_nonincr_vs_incr = "figure_cum_distr_incr.pdf"
    outfile_incr_vs_incrrel = "figure_cum_distr_rel.pdf"
    #outfile_nonincr_vs_incr = "figure_cum_distr_incr_cfungr0.pdf"
    #outfile_incr_vs_incrrel = "figure_cum_distr_rel_cfungr0.pdf"

    min = df[[header_runtime_parent, header_runtime_incr_child]].min().min()
    max = df[[header_runtime_parent, header_runtime_incr_child]].max().max()
    bins = np.linspace(min,max,num=num_bins+1)
    non_incr_values, non_incr_base = np.histogram(df.loc[:,header_runtime_parent], bins=bins)
    incr_values, incr_base = np.histogram(df.loc[:,header_runtime_incr_child], bins=bins)
    cum_non_incr = np.cumsum(non_incr_values, dtype=np.float)
    cum_incr = np.cumsum(incr_values, dtype=np.float)
    cum_non_incr[cum_non_incr==0] = np.nan
    cum_incr[cum_incr==0] = np.nan

    plt.figure()
    plt.xlabel('Number of Commits with runtime $\leq$ y')
    plt.ylabel('Runtime in s')
    plt.tight_layout()
    plt.plot(cum_non_incr, non_incr_base[:-1], c="blue", label="Non-incremental analysis of parent commit")
    plt.plot(cum_incr, non_incr_base[:-1], c="red", label="Incremental analysis of commit")
    plt.legend()
    plt.savefig(outfile_nonincr_vs_incr)

    min = df[[header_runtime_incr_child, header_runtime_incr_rel_child]].min().min()
    max = df[[header_runtime_incr_child, header_runtime_incr_rel_child]].max().max()
    bins2 = np.linspace(min,max,num=num_bins+1)
    incr_values, incr_base = np.histogram(df.loc[:,header_runtime_incr_child], bins=bins2)
    incr_rel_values, incr_rel_base = np.histogram(df.loc[:,header_runtime_incr_rel_child], bins=bins2)
    cum_incr = np.cumsum(incr_values, dtype=np.float)
    cum_incr_rel = np.cumsum(incr_rel_values, dtype=np.float)
    cum_incr[cum_incr==0] = np.nan
    cum_incr_rel[cum_incr_rel==0] = np.nan
    plt.figure()
    plt.xlabel('Number of Commits with runtime $\leq$ y')
    plt.ylabel('Runtime in s ($log_2$ scale)')
    plt.yscale('log', base=2)
    plt.gca().yaxis.set_major_formatter(ScalarFormatter())
    plt.tight_layout()
    plt.plot(cum_incr, incr_base[:-1], c="red", label="Incremental")
    plt.plot(cum_incr_rel, incr_base[:-1], c="green", label="Incremental + Reluctant")
    plt.legend()
    plt.savefig(outfile_incr_vs_incrrel)
