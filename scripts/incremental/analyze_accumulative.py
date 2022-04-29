import utils
from pydriller import Repository, Git
import psutil
import os
import sys
from datetime import datetime
import json
import shutil
import pytz
import multiprocessing as mp


### Usage: python3 incremental_smallcommits.py <full_path_analyzer_dir> <repo_url> <repo_name> <name_of_build_script>
#     <name_of_config> <begin> <from_commit_index> <to_commit_index>
# Executing the script will overwrite the directory 'result' in the cwd.
# The script for building the compilation database is assumed to be found in the analyzers script directory and the
# config file is assumed to be found in the conf directory of the analyzers repository.
if len(sys.argv) != 3:
      print("Wrong number of parameters.\nUse script like this: python3 parallel_benchmarking.py <path to goblint directory> <number of processes>")
      exit()
maxCLOC       = None
url           = "https://github.com/facebook/zstd"
repo_name     = "zstd"
build_compdb  = "build_compdb_zstd.sh"
conf          = "big-benchmarks1"
begin         = datetime(2021,2,1)
to            = datetime(2022,2,1)
diff_exclude  = ["build", "doc", "examples", "tests", "zlibWrapper", "contrib"]
analyzer_dir  = sys.argv[1]
try:
    numcores = int(sys.argv[2])
except ValueError:
    print("Parameter should be a number.\nUse script like this: python3 parallel_benchmarking.py <path to goblint directory> <number of processes>")
    exit()
################################################################################

res_dir = os.path.abspath('result')
utc = pytz.UTC

def start_commit_for_sequence_search():
    current_commit = ""
    for commit in Repository(url, to=to, only_in_branch='dev', order='reverse', clone_repo_to=res_dir).traverse_commits():
        current_commit = commit
        break
    gr = Git(os.path.join(res_dir, repo_name))
    return current_commit, gr

def find_sequences_rec(gr, commit, seq, seq_list, starting_points, vis):
    for p in commit.parents:
        if p in vis:
            continue
        parent_commit = gr.get_commit(p)
        parent_date = parent_commit.committer_date.replace(tzinfo=None)
        if parent_date < begin:
            if len(seq) > 5:
                print("found seq of length: " + str(len(seq)))
                seq_list.insert(0,seq)
        elif parent_commit.merge:
            if len(seq) > 5:
                print("found seq of length: " + str(len(seq)))
                seq_list.insert(0,seq)
            find_sequences_rec(gr, parent_commit, [], seq_list, starting_points, [])
        else:
            seq.insert(0,parent_commit.hash)
            if parent_commit.hash not in starting_points:
                starting_points.insert(0,parent_commit.hash)
                find_sequences_rec(gr, parent_commit, seq, seq_list, starting_points, vis)


def find_sequences():
    seq_list = []
    starting_points=[]
    start_commit, gr = start_commit_for_sequence_search()
    starting_points.insert(0,start_commit.hash)
    find_sequences_rec(gr, start_commit, [], seq_list, starting_points, [])
    print("summary")
    total = 0
    maxlen = max(map(lambda x : len(x), seq_list))
    for i in range(0,maxlen + 1):
        c = sum(map(lambda x : len(x) == i, seq_list))
        total += c
        print("length " + str(i) + ": " + str(c))
    print("total: " + str(len(seq_list)))
    assert(total == len(seq_list))
    with open('sequences.json', 'w') as outfile:
        json.dump(seq_list, outfile, indent=4)
    return seq_list

def analyze_series_in_repo(series):
    prev_commit = ""
    commit_num = 0
    repo_path = os.path.abspath(repo_name)
    out_dir = os.path.abspath('out')
    with open('sequence.json', 'w') as outfile:
        json.dump(series, outfile, indent=4)
    dummy_c_file = "file.c"
    with open(dummy_c_file, 'w') as file:
        file.write("int main() { return 0; }")
        file.close()

    for commit in Repository(url, since=begin, only_commits=series, clone_repo_to=os.getcwd()).traverse_commits():
        gr = Git(repo_path)

        print("\n" + commit.hash)
        print('changed LOC: ', commit.lines)
        print('merge commit: ', commit.merge)

        # check that given series is a path of sequential commits in the repository
        assert(prev_commit == "" or prev_commit in commit.parents)

        relCLOC = utils.calculateRelCLOC(repo_path, commit, diff_exclude)

        # analyze
        out_commit = os.path.join(out_dir, str(commit_num))
        os.makedirs(out_commit)
        with open(os.path.join(out_commit,'commit_properties.log'), "w+") as file:
            json.dump({"hash": commit.hash, "parent_hash": prev_commit, "CLOC": commit.lines, "relCLOC": relCLOC}, file)

        if commit_num == 0:
            # analyze initial commit non-incrementally
            try:
                print('Analyze ', str(commit.hash), ' as initial commit.')
                add_options = ['--disable', 'incremental.load', '--enable', 'incremental.save', '--enable', 'incremental.verify']
                utils.analyze_commit(analyzer_dir, gr, repo_path, build_compdb, commit.hash, out_commit, conf, add_options)
                prev_commit = commit.hash
            except utils.subprocess.CalledProcessError as e:
                print('Aborted initial because command ', e.cmd, 'failed.')
                print('Fix the problem or choose a different commit to start the accumulative analysis from')
                exit()
        else:
            # analyze every following commit based on the latest previous commit for which the analysis succeeded
            try:
                if os.path.isdir("backup_incremental_data"):
                    shutil.rmtree("backup_incremental_data")
                shutil.copytree("incremental_data", "backup_incremental_data")

                # compare only for 10th and last run
                if commit_num == 10 or commit_num == len(series) - 1:
                    # analyze commit non-incrementally and save run for comparison
                    print('Analyze', str(commit.hash), 'non-incrementally (#', commit_num, ').')
                    out_nonincr = os.path.join(out_commit, 'non-incr')
                    os.makedirs(out_nonincr)
                    file_original_run = os.path.join(out_nonincr, "compare-data-nonincr")
                    add_options = ['--enable', 'incremental.only-rename', '--enable', 'incremental.verify', '--set', 'save_run', file_original_run]
                    utils.analyze_commit(analyzer_dir, gr, repo_path, build_compdb, commit.hash, out_nonincr, conf, add_options)

                # analyze commit incrementally based on the previous commit and save run for comparison
                print('Analyze', str(commit.hash), 'incrementally (#', commit_num, ').')
                out_incr = os.path.join(out_commit, 'incr')
                os.makedirs(out_incr)
                file_incremental_run = os.path.join(out_incr, "compare-data-incr")
                add_options = ['--enable', 'incremental.load', '--enable', 'incremental.save', '--enable', 'incremental.reluctant.on', '--enable', 'incremental.verify', '--set', 'save_run', file_incremental_run]
                utils.analyze_commit(analyzer_dir, gr, repo_path, build_compdb, commit.hash, out_incr, conf, add_options)

                if commit_num == 10 or commit_num == len(series):
                    # compare stored data of original and incremental run
                    print('Compare both runs.')
                    out_compare = os.path.join(out_commit, 'compare')
                    os.makedirs(out_compare)
                    utils.compare_runs(analyzer_dir, dummy_c_file, out_compare, conf, file_incremental_run, file_original_run)

                prev_commit = commit.hash
            except utils.subprocess.CalledProcessError as e:
                print('Aborted because command ', e.cmd, 'failed.')
                shutil.rmtree("incremental_data")
                shutil.copytree("backup_incremental_data", "incremental_data")
            commit_num += 1

def runperprocess(core, serie):
    psutil.Process().cpu_affinity([core])
    analyze_series_in_repo(serie)

def analyze_seq_in_parallel(series):
    avail_phys_cores = psutil.cpu_count(logical=False)
    allowedcores = avail_phys_cores - 2
    if numcores > allowedcores:
        print("Not enough physical cores on this maching (exist: ", avail_phys_cores, " allowed: ", allowedcores, ")")
        exit()
    # For equal load distribution, choose a processes to core mapping,
    # use only physical cores and have an equal number of processes per cache.
    # The layout of physical/logical cores and sharing of caches is machine dependent. To find out use: 'lscpu --all --extended'.
    # For our test server:
    coremapping1 = [i for i in range(numcores - numcores//2)]
    coremapping2 = [i for i in range(avail_phys_cores//2, avail_phys_cores//2 + numcores//2)]
    coremapping = [val for pair in zip(coremapping1, coremapping2) for val in pair]
    processes = []

    for i in range(len(series)):
        dir = "process" + str(i)
        os.mkdir(dir)
        os.chdir(dir)
        # start process for analysing serie i
        p = mp.Process(target=runperprocess, args=[coremapping[i % numcores], series[i]])
        p.start()
        processes.append(p)
        os.chdir(res_dir)

    for p in processes:
        p.join()

if os.path.exists(res_dir):
    shutil.rmtree(res_dir)
os.mkdir(res_dir)
os.chdir(res_dir)

print("find sequences to analyze")
seq_list = find_sequences()

print("\nanalyze sequences in parallel")
analyze_seq_in_parallel(seq_list)
