from pydriller import Repository, Git
import utils
import psutil
import multiprocessing as mp
import os
import subprocess
import itertools
import shutil
import json
from datetime import datetime
import sys
import pandas as pd

################################################################################
# Usage: python3 incremental_smallcommits.py <full_path_analyzer_dir> <number_of_cores>
# Executing the script will overwrite the directory 'result_efficiency' in the cwd.
# The script for building the compilation database is assumed to be found in the analyzers script directory and the
# config file is assumed to be found in the conf directory of the analyzers repository.
# The single test runs are mapped to processors according to the coremapping. The one specified in the section below
# should work for Intel machines, otherwise you might need to adapt it according to the description.
if len(sys.argv) != 3:
      print("Wrong number of parameters.\nUse script like this: python3 parallel_benchmarking.py <path to goblint directory> <number of processes>")
      exit()
result_dir    = os.path.join(os.getcwd(), 'result_efficiency')
maxCLOC       = 50 # can be deactivated with None
url           = "https://github.com/facebook/zstd"
repo_name     = "zstd"
build_compdb  = "build_compdb_zstd.sh"
conf_base     = "zstd-race-baseline" # very minimal: "zstd-minimal"
conf_incrpost = "zstd-race-incrpostsolver"
begin         = datetime(2021,8,1)
to            = datetime(2022,2,1) # minimal subset: datetime(2021,8,4)
diff_exclude  = ["build", "doc", "examples", "tests", "zlibWrapper", "contrib"]
analyzer_dir  = sys.argv[1]
only_collect_results = False # can be turned on to collect results, if data collection was aborted before the creation of result tables
################################################################################
try:
    numcores = int(sys.argv[2])
except ValueError:
    print("Parameter should be a number.\nUse script like this: python3 parallel_benchmarking.py <absolute path to goblint directory> <number of processes>")
    exit()
avail_phys_cores = psutil.cpu_count(logical=False)
allowedcores = avail_phys_cores - 1
if not only_collect_results and numcores > allowedcores:
    print("Not enough physical cores on this machine (exist: ", avail_phys_cores, " allowed: ", allowedcores, ")")
    exit()
# For equal load distribution, choose a processes to core mapping,
# use only physical cores and have an equal number of processes per cache.
# The layout of physical/logical cores and sharing of caches is machine dependent. To find out use: 'lscpu --all --extended'.
# For our test server:
coremapping1 = [i for i in range(numcores - numcores//2)]
coremapping2 = [i for i in range(avail_phys_cores//2, avail_phys_cores//2 + numcores//2)]
coremapping = [coremapping1[i//2] if i%2==0 else coremapping2[i//2] for i in range(len(coremapping1) + len(coremapping2))]
################################################################################

def filter_commits_false_pred(repo_path):
    def pred(c):
        relCLOC = utils.calculateRelCLOC(repo_path, c, diff_exclude)
        return relCLOC == 0 or (maxCLOC is not None and relCLOC > maxCLOC)
    return pred

def analyze_small_commits_in_repo(cwd, outdir, from_c, to_c):
    count_analyzed = 0
    count_skipped = 0
    count_failed = 0
    analyzed_commits = {}
    repo_path = os.path.join(cwd, repo_name)

    for commit in itertools.islice(itertools.filterfalse(filter_commits_false_pred(repo_path), Repository(url, since=begin, to=to, only_no_merge=True, clone_repo_to=cwd).traverse_commits()), from_c, to_c):
        gr = Git(repo_path)

        #print("\n" + commit.hash)
        #print('changed LOC: ', commit.lines)
        #print('merge commit: ', commit.merge)

        # skip merge commits and commits that have no or less than maxCLOC of relevant code changes
        relCLOC = utils.calculateRelCLOC(repo_path, commit, diff_exclude) # use this to filter commits by actually relevant changes
        #print("relCLOC: ", relCLOC)
        if relCLOC == 0 or (maxCLOC is not None and relCLOC > maxCLOC):
            #print('Skip this commit: merge commit or too many relevant changed LOC')
            count_skipped+=1
            continue

        # analyze
        try_num = from_c + count_analyzed + count_failed + 1
        outtry = os.path.join(outdir, str(try_num))
        parent = gr.get_commit(commit.parents[0])
        #print('Analyze this commit incrementally. #', try_num)

        utils.reset_incremental_data(os.path.join(cwd, 'incremental_data'))
        failed = True
        try:
            #print('Starting from parent', str(parent.hash), ".")
            outparent = os.path.join(outtry, 'parent')
            os.makedirs(outparent)
            add_options = ['--disable', 'incremental.load', '--enable', 'incremental.save']
            utils.analyze_commit(analyzer_dir, gr, repo_path, build_compdb, parent.hash, outparent, conf_base, add_options)

            #print('And now analyze', str(commit.hash), 'incrementally.')
            outchild = os.path.join(outtry, 'child')
            os.makedirs(outchild)
            add_options = ['--enable', 'incremental.load', '--disable', 'incremental.save']
            utils.analyze_commit(analyzer_dir, gr, repo_path, build_compdb, commit.hash, outchild, conf_base, add_options)

            #print('And again incremental, this time with incremental postsolver')
            outchildincrpost = os.path.join(outtry, 'child-incr-post')
            os.makedirs(outchildincrpost)
            add_options = ['--enable', 'incremental.load', '--disable', 'incremental.save']
            utils.analyze_commit(analyzer_dir, gr, repo_path, build_compdb, commit.hash, outchildincrpost, conf_incrpost, add_options)

            #print('And again incremental, this time with incremental postsolver and reluctant')
            outchildrel = os.path.join(outtry, 'child-rel')
            os.makedirs(outchildrel)
            add_options = ['--enable', 'incremental.load', '--disable', 'incremental.save', '--enable', 'incremental.reluctant.enabled']
            utils.analyze_commit(analyzer_dir, gr, repo_path, build_compdb, commit.hash, outchildrel, conf_incrpost, add_options)

            count_analyzed+=1
            failed = False
        except subprocess.CalledProcessError as e:
            print('Aborted because command ', e.cmd, 'failed.')
            count_failed+=1
        os.makedirs(outtry, exist_ok=True)
        with open(os.path.join(outtry,'commit_properties.log'), "w+") as file:
            json.dump({"hash": commit.hash, "parent_hash": parent.hash, "CLOC": commit.lines, "relCLOC": relCLOC, "failed": failed}, file)
        analyzed_commits[try_num]=(str(commit.hash)[:6], relCLOC)

    num_commits = count_analyzed + count_skipped + count_failed
    print("\nCommits traversed in total: ", num_commits)
    print("Analyzed: ", count_analyzed)
    print("Failed: ", count_failed)
    print("Skipped: ", count_skipped)

def collect_data(outdir):
    data = {"Commit": [], "Failed?": [], "Changed LOC": [], "Relevant changed LOC": [], "Changed/Added/Removed functions": [],
      utils.header_runtime_parent: [], utils.header_runtime_incr_child: [],
      utils.header_runtime_incr_posts_child: [], utils.header_runtime_incr_posts_rel_child: [],
      "Change in number of race warnings": []}
    for t in os.listdir(outdir):
        parentlog = os.path.join(outdir, t, 'parent', utils.analyzerlog)
        childlog = os.path.join(outdir, t, 'child', utils.analyzerlog)
        childpostslog = os.path.join(outdir, t, 'child-incr-post', utils.analyzerlog)
        childpostsrellog = os.path.join(outdir, t, 'child-rel', utils.analyzerlog)
        commit_prop_log = os.path.join(outdir, t, 'commit_properties.log')
        t = int(t)
        commit_prop = json.load(open(commit_prop_log, "r"))
        data["Changed LOC"].append(commit_prop["CLOC"])
        data["Relevant changed LOC"].append(commit_prop["relCLOC"])
        data["Failed?"].append(commit_prop["failed"])
        data["Commit"].append(commit_prop["hash"][:7])
        if commit_prop["failed"] == True:
            data[utils.header_runtime_parent].append(0)
            data[utils.header_runtime_incr_child].append(0)
            data[utils.header_runtime_incr_posts_child].append(0)
            data[utils.header_runtime_incr_posts_rel_child].append(0)
            data["Changed/Added/Removed functions"].append(0)
            data["Change in number of race warnings"].append(0)
            continue
        parent_info = utils.extract_from_analyzer_log(parentlog)
        child_info = utils.extract_from_analyzer_log(childlog)
        child_posts_info = utils.extract_from_analyzer_log(childpostslog)
        child_posts_rel_info = utils.extract_from_analyzer_log(childpostsrellog)
        data["Changed/Added/Removed functions"].append(int(child_info["changed"]) + int(child_info["added"]) + int(child_info["removed"]))
        data[utils.header_runtime_parent].append(float(parent_info["runtime"]))
        data[utils.header_runtime_incr_child].append(float(child_info["runtime"]))
        data[utils.header_runtime_incr_posts_child].append(float(child_posts_info["runtime"]))
        data[utils.header_runtime_incr_posts_rel_child].append(float(child_posts_rel_info["runtime"]))
        data["Change in number of race warnings"].append(int(child_info["race_warnings"] - int(parent_info["race_warnings"])))
    return data

def runperprocess(core, from_c, to_c):
    if not only_collect_results:
        psutil.Process().cpu_affinity([core])
    cwd  = os.getcwd()
    outdir = os.path.join(cwd, 'out')
    if not only_collect_results:
        if os.path.exists(outdir) and os.path.isdir(outdir):
          shutil.rmtree(outdir)
        analyze_small_commits_in_repo(cwd, outdir, from_c, to_c)
    data_set = collect_data(outdir)
    df = pd.DataFrame(data_set)
    #df.sort_index(inplace=True, key=lambda idx: idx.map(lambda x: int(x.split(":")[0])))
    print(df)
    df.to_csv('results.csv', sep =';')

def analyze_chunks_of_commits_in_parallel():
    processes = []

    # calculate actual number of interesting commits up-front to allow for similar load distribution
    iter = itertools.filterfalse(filter_commits_false_pred(os.path.join(os.getcwd(), repo_name)), Repository(url, since=begin, to=to, only_no_merge=True, clone_repo_to=os.getcwd()).traverse_commits())
    num_commits = sum(1 for _ in iter)
    print("Number of potentially interesting commits:", num_commits)
    perprocess = num_commits // numcores if num_commits % numcores == 0 else num_commits // numcores + 1
    print("Per process: " + str(perprocess))

    for i in range(numcores):
        dir = "process" + str(i)
        if not only_collect_results:
            os.mkdir(dir)
        os.chdir(dir)
        # run script
        start = perprocess * i
        end = perprocess * (i + 1) if i < numcores - 1 else num_commits
        if not only_collect_results:
            p = mp.Process(target=runperprocess, args=[coremapping[i], start, end])
            p.start()
            processes.append(p)
            # time.sleep(random.randint(5,60)) # add random delay between process creation to try to reduce interference
        else:
            runperprocess(coremapping[i], start, end)
        os.chdir(result_dir)

    for p in processes:
        p.join()

def merge_results():
    filename = "results.csv"
    frames = []
    for process_dir in os.listdir("."):
        path = os.path.join(process_dir, filename)
        if os.path.exists(path):
            t = pd.read_csv(path, index_col=0, sep=";")
            frames.append(t)
    if len(frames) > 0:
        df = pd.concat(frames)
        #df.sort_index(inplace=True, key=lambda idx: idx.map(lambda x: int(x.split(":")[0])))
        df.to_csv('total_results.csv', sep=";")


if not only_collect_results:
    os.mkdir(result_dir)
os.chdir(result_dir)

analyze_chunks_of_commits_in_parallel()
merge_results()
