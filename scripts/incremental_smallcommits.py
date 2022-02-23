from pydriller import Repository, Git
import os
import sys
from pathlib import Path
import datetime
import subprocess
import shutil
import re
import json
import pandas as pd
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

if(sys.argv[0] != 'scripts/incremental_smallcommits.py'):
  sys.exit("The script must be run from the analyzers base directory!")

##################### Repo specific arguments ##################################
maxCLOC       = 100
conf          = "big-benchmarks2"
repo_name     = "zstd"
repo_rel_path = "../test-repos/" + repo_name
build_compdb  = 'build_compdb_zstd.sh'
analyzer_dir  = os.getcwd()
beginwith     = datetime.datetime(2021, 6, 1)
################################################################################


outdir = os.path.join(analyzer_dir, 'out', repo_name)
repo_path = os.path.normpath(os.path.join(analyzer_dir, repo_rel_path))
gr = Git(repo_path)

analyzed_commits = {}
count_analyzed = 0
count_skipped = 0
count_failed = 0

def clean_test_repo():
    print('Cleanup test repository')
    gr.clear()
    subprocess.run(['make', 'clean'], cwd = repo_path, check=True)

def reset_incremental_data():
    incr_data_dir = os.path.join(analyzer_dir, 'incremental_data')
    if os.path.exists(incr_data_dir) and os.path.isdir(incr_data_dir):
        shutil.rmtree(incr_data_dir)

def analyze_commit(commit_hash, outdir):
    gr.checkout(commit_hash)

    prepare_command = ['sh', analyzer_dir + '/scripts/' + build_compdb]
    with open(outdir+'/prepare.log', "w+") as outfile:
        output = subprocess.run(prepare_command, cwd = repo_path, check=True, stdout=outfile, stderr=subprocess.STDOUT)
        outfile.close()

    analyze_command = ['./goblint', '--conf', 'conf/' + conf + '.json', repo_path]
    with open(outdir+'/analyzer.log', "w+") as outfile:
        output = subprocess.run(analyze_command, check=True, stdout=outfile, stderr=subprocess.STDOUT)
        outfile.close()

def calculateRelCLOC(commit):
    diff_exclude = ["build", "doc", "examples", "tests", "zlibWrapper", "contrib"]
    diff_exclude = map(lambda x: os.path.join(repo_path, x), diff_exclude)
    relcloc = 0
    for f in commit.modified_files:
        _, extension = os.path.splitext(f.filename)
        if not (extension == ".h" or extension == ".c"):
            continue
        parents = Path(f.new_path).parents
        if any(dir in parents for dir in diff_exclude):
            continue
        relcloc = relcloc + f.added_lines + f.deleted_lines
    return relcloc

def analyze_small_commits_in_repo():
    global count_analyzed
    global count_skipped
    global count_failed
    global analyzed_commits

    for commit in Repository('https://github.com/facebook/zstd', since=beginwith).traverse_commits():
        print("\n" + commit.hash)
        print('changed LOC: ', commit.lines)
        print('merge commit: ', commit.merge)

        # skip merge commits and commits that have less than maxCLOC of relevant code changes
        relCLOC = calculateRelCLOC(commit) # use this to filter commits by actually relevant changes
        print("relCLOC: ", relCLOC)
        if commit.merge or relCLOC > maxCLOC:
            print('Skip this commit: merge commit or too many relevant changed LOC')
            count_skipped+=1
            continue

        # analyze
        try_num = count_analyzed + count_failed + 1
        outtry = os.path.join(outdir, str(try_num))
        parent = gr.get_commit(commit.parents[0])
        os.makedirs(outtry)
        with open(os.path.join(outtry,'commit_properties.log'), "w+") as file:
            json.dump({"hash": commit.hash, "parent_hash": parent.hash, "CLOC": commit.lines, "relCLOC": relCLOC}, file)
        print('Analyze this commit incrementally. #', try_num)

        reset_incremental_data()

        try:
            print('Starting from parent', str(parent.hash), ".")
            outparent = os.path.join(outtry, 'parent')
            os.makedirs(outparent)
            clean_test_repo()
            analyze_commit(parent.hash, outparent)

            print('And now analyze', str(commit.hash), 'incrementally.')
            outchild = os.path.join(outtry, 'child')
            os.makedirs(outchild)
            clean_test_repo()
            analyze_commit(commit.hash, outchild)

            count_analyzed+=1
        except subprocess.CalledProcessError as e:
            print('Aborted because command ', e.cmd, 'failed.')
            count_failed+=1
        analyzed_commits[try_num]=(str(commit.hash)[:6], relCLOC)


def extract_from_analyzer_log(log):
    def find_line(pattern):
        file = open(log, "r")
        for line in file.readlines():
            m = re.search(pattern, line)
            if m:
                return m.groupdict()
        file.close()
    runtime_pattern = 'TOTAL[ ]+(?P<runtime>[0-9\.]+) s'
    change_info_pattern = 'change_info = { unchanged = (?P<unchanged>[0-9]*); changed = (?P<changed>[0-9]*); added = (?P<added>[0-9]*); removed = (?P<removed>[0-9]*) }'
    return dict(list(find_line(runtime_pattern).items()) + list(find_line(change_info_pattern).items()))


def collect_data():
    index = []
    data = {"Runtime for parent commit (non-incremental)": [], "Runtime for commit (incremental)": [],
        "Changed/Added/Removed functions": [], "Changed LOC": [], "Relevant changed LOC": []}
    for t in os.listdir(outdir):
        parentlog = os.path.join(outdir, t, 'parent', 'analyzer.log')
        childlog = os.path.join(outdir, t, 'child', 'analyzer.log')
        commit_prop_log = os.path.join(outdir, t, 'commit_properties.log')
        t = int(t)
        commit_prop = json.load(open(commit_prop_log, "r"))
        data["Changed LOC"].append(commit_prop["CLOC"])
        data["Relevant changed LOC"].append(commit_prop["relCLOC"])
        index.append(str(t) + ": " + commit_prop["hash"][:7])
        if not os.path.exists(parentlog) or not os.path.exists(childlog):
            data["Runtime for parent commit (non-incremental)"].append(0)
            data["Runtime for commit (incremental)"].append(0)
            data["Changed/Added/Removed functions"].append(0)
            continue
        parent_info = extract_from_analyzer_log(parentlog)
        child_info = extract_from_analyzer_log(childlog)
        data["Runtime for parent commit (non-incremental)"].append(float(parent_info["runtime"]))
        data["Runtime for commit (incremental)"].append(float(child_info["runtime"]))
        data["Changed/Added/Removed functions"].append(int(child_info["changed"]) + int(child_info["added"]) + int(child_info["removed"]))
    return {"index": index, "data": data}

def plot(data_set):
    df = pd.DataFrame(data_set["data"], index=data_set["index"]) # TODO: index=analyzed_commits
    df.sort_index(inplace=True, key=lambda idx: idx.map(lambda x: int(x.split(":")[0])))
    print(df)

    df.plot.bar(rot=0, width=1, figsize=(25,5))
    plt.xticks(rotation=45, ha='right', rotation_mode='anchor')
    plt.xlabel('Commit')
    plt.tight_layout()
    plt.savefig("figure.pdf")


if os.path.exists(outdir) and os.path.isdir(outdir):
   shutil.rmtree(outdir)
analyze_small_commits_in_repo()
num_commits = count_analyzed + count_skipped
print("Commits traversed in total: ", num_commits)
print("Analyzed: ", count_analyzed)
print("Failed: ", count_failed)
print("Skipped: ", count_skipped)

data = collect_data()
plot(data)
