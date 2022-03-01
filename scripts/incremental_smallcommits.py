from pydriller import Repository, Git
import os
import sys
from pathlib import Path
import subprocess
import itertools
from datetime import datetime
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

### Usage: python3 incremental_smallcommits.py <full_path_analyzer_dir> <repo_url> <repo_name> <name_of_build_script>
#     <name_of_config> <begin> <from_commit_index> <to_commit_index>
# Executing the script will overwrite the directory 'out' in the cwd.
# The script for building the compilation database is assumed to be found in the analyzers script directory and the
# config file is assumed to be found in the conf directory of the analyzers repository.
maxCLOC       = 50
analyzer_dir  = sys.argv[1]
url           = sys.argv[2]
repo_name     = sys.argv[3]
build_compdb  = sys.argv[4]
conf          = sys.argv[5]
begin         = datetime.strptime(sys.argv[6], '%Y/%m/%d')
from_c        = int(sys.argv[7])
to_c          = int(sys.argv[8])
################################################################################

cwd  = os.getcwd()
outdir = os.path.join(cwd, 'out')
repo_path = os.path.normpath(os.path.join(cwd, repo_name))

analyzed_commits = {}
count_analyzed = 0
count_skipped = 0
count_failed = 0

def clean_test_repo(gr):
    print('Cleanup test repository')
    gr.clear()
    subprocess.run(['make', 'clean'], cwd = repo_path, check=True)

def reset_incremental_data():
    incr_data_dir = os.path.join(cwd, 'incremental_data')
    if os.path.exists(incr_data_dir) and os.path.isdir(incr_data_dir):
        shutil.rmtree(incr_data_dir)

def analyze_commit(gr, commit_hash, outdir):
    gr.checkout(commit_hash)

    prepare_command = ['sh', os.path.join(analyzer_dir, 'scripts', build_compdb)]
    with open(outdir+'/prepare.log', "w+") as outfile:
        subprocess.run(prepare_command, cwd = repo_path, check=True, stdout=outfile, stderr=subprocess.STDOUT)
        outfile.close()

    analyze_command = [os.path.join(analyzer_dir, 'goblint'), '--conf', os.path.join(analyzer_dir, 'conf', conf + '.json'), repo_path]
    with open(outdir+'/analyzer.log', "w+") as outfile:
        subprocess.run(analyze_command, check=True, stdout=outfile, stderr=subprocess.STDOUT)
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

    for commit in itertools.islice(Repository(url, since=begin, only_no_merge=True, clone_repo_to=cwd).traverse_commits(), from_c, to_c):
        gr = Git(repo_path)

        print("\n" + commit.hash)
        print('changed LOC: ', commit.lines)
        print('merge commit: ', commit.merge)

        # skip merge commits and commits that have less than maxCLOC of relevant code changes
        relCLOC = calculateRelCLOC(commit) # use this to filter commits by actually relevant changes
        print("relCLOC: ", relCLOC)
        if relCLOC > maxCLOC:
            print('Skip this commit: merge commit or too many relevant changed LOC')
            count_skipped+=1
            continue

        # analyze
        try_num = from_c + count_analyzed + count_failed + 1
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
            clean_test_repo(gr)
            analyze_commit(gr, parent.hash, outparent)

            print('And now analyze', str(commit.hash), 'incrementally.')
            outchild = os.path.join(outtry, 'child')
            os.makedirs(outchild)
            clean_test_repo(gr)
            analyze_commit(gr, commit.hash, outchild)

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
                file.close()
                return m.groupdict()
    runtime_pattern = 'TOTAL[ ]+(?P<runtime>[0-9\.]+) s'
    change_info_pattern = 'change_info = { unchanged = (?P<unchanged>[0-9]*); changed = (?P<changed>[0-9]*); added = (?P<added>[0-9]*); removed = (?P<removed>[0-9]*) }'
    d = dict(list(find_line(runtime_pattern).items()) + list(find_line(change_info_pattern).items()))
    file = open(log, "r")
    num_racewarnings = file.read().count('[Warning][Race]')
    d["race_warnings"] = num_racewarnings
    file.close()
    return d


def collect_data():
    index = []
    data = {"Changed LOC": [], "Relevant changed LOC": [], "Changed/Added/Removed functions": [],
      "Runtime for parent commit (non-incremental)": [], "Runtime for commit (incremental)": [],
      "Change in number of race warnings": []}
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
        data["Changed/Added/Removed functions"].append(int(child_info["changed"]) + int(child_info["added"]) + int(child_info["removed"]))
        data["Runtime for parent commit (non-incremental)"].append(float(parent_info["runtime"]))
        data["Runtime for commit (incremental)"].append(float(child_info["runtime"]))
        data["Change in number of race warnings"].append(int(parent_info["race_warnings"]) - int(child_info["race_warnings"]))
    return {"index": index, "data": data}

def plot(data_set):
    df = pd.DataFrame(data_set["data"], index=data_set["index"]) # TODO: index=analyzed_commits
    df.sort_index(inplace=True, key=lambda idx: idx.map(lambda x: int(x.split(":")[0])))
    print(df)
    df.to_csv('results.csv')

    df.plot.bar(rot=0, width=0.8, figsize=(25,10))
    plt.xticks(rotation=45, ha='right', rotation_mode='anchor')
    plt.xlabel('Commit')
    plt.tight_layout()
    plt.savefig("figure.pdf")


if os.path.exists(outdir) and os.path.isdir(outdir):
   shutil.rmtree(outdir)
analyze_small_commits_in_repo()
num_commits = count_analyzed + count_skipped
print("\nCommits traversed in total: ", num_commits)
print("Analyzed: ", count_analyzed)
print("Failed: ", count_failed)
print("Skipped: ", count_skipped)

data = collect_data()
plot(data)
