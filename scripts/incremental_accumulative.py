import incremental_benchmark_utils as utils
from pydriller import Repository, Git
import itertools
import os
import sys
from datetime import datetime
import json
import shutil

### Usage: python3 incremental_smallcommits.py <full_path_analyzer_dir> <repo_url> <repo_name> <name_of_build_script>
#     <name_of_config> <begin> <from_commit_index> <to_commit_index>
# Executing the script will overwrite the directory 'out' in the cwd.
# The script for building the compilation database is assumed to be found in the analyzers script directory and the
# config file is assumed to be found in the conf directory of the analyzers repository.
maxCLOC       = None
analyzer_dir  = sys.argv[1]
url           = "https://github.com/facebook/zstd" #sys.argv[2]
repo_name     = "zstd" #sys.argv[3]
build_compdb  = "build_compdb_zstd.sh" #sys.argv[4]
conf          = "big-benchmarks1" #sys.argv[5]
begin         = datetime(2021,2,1) #datetime.strptime(sys.argv[6], '%Y/%m/%d')
from_c        = 0 #int(sys.argv[7])
to_c          = None #int(sys.argv[8])
diff_exclude  = ["build", "doc", "examples", "tests", "zlibWrapper", "contrib"]
################################################################################

cwd  = os.getcwd()
outdir = os.path.join(cwd, 'out')
repo_path = os.path.normpath(os.path.join(cwd, repo_name))

analyzed_commits = {}
count_analyzed = 0
count_skipped = 0
count_failed = 0

dummy_c_file = "file.c"
with open(dummy_c_file, 'w') as file:
  file.write("int main() { return 0; }")
  file.close()


def analyze_series_in_repo():
    global count_analyzed
    global count_skipped
    global count_failed
    global analyzed_commits

    prev_commit = ""

    for index, commit in enumerate(itertools.islice(Repository(url, since=begin, only_no_merge=True, only_in_branch='dev', clone_repo_to=cwd).traverse_commits(), from_c, to_c)):
        gr = Git(repo_path)

        print("\n" + commit.hash)
        print('changed LOC: ', commit.lines)
        print('merge commit: ', commit.merge)

        # skip merge commits and commits that have less than maxCLOC of relevant code changes
        relCLOC = utils.calculateRelCLOC(repo_path, commit, diff_exclude) # use this to filter commits by actually relevant changes
        print("relCLOC: ", relCLOC)
        if maxCLOC is not None and relCLOC > maxCLOC:
            print('Skip this commit: merge commit or too many relevant changed LOC')
            count_skipped+=1
            continue

        # analyze
        try_num = from_c + count_analyzed + count_failed + 1
        outtry = os.path.join(outdir, str(try_num))
        os.makedirs(outtry)
        with open(os.path.join(outtry,'commit_properties.log'), "w+") as file:
            json.dump({"hash": commit.hash, "parent_hash": prev_commit, "CLOC": commit.lines, "relCLOC": relCLOC}, file)

        if index == 0:
            # analyze initial commit non-incrementally
            try:
                print('Analyze ', str(commit.hash), ' as initial commit.')
                add_options = ['--disable', 'incremental.load', '--enable', 'incremental.save']
                utils.analyze_commit(analyzer_dir, gr, repo_path, build_compdb, commit.hash, outtry, conf, add_options)
                count_analyzed += 1
                prev_commit = commit.hash
            except utils.subprocess.CalledProcessError as e:
                print('Aborted initial because command ', e.cmd, 'failed.')
                print('Fix the problem or choose a different commit to start the accumulative analysis from')
                exit()
        else:
            # analyze every following commit based on the latest previous commit for which the analysis succeeded
            try:
                print('Analyze', str(commit.hash), 'incrementally (#', try_num, ').')
                if os.path.isdir("backup_incremental_data"):
                    shutil.rmtree("backup_incremental_data")
                shutil.copytree("incremental_data", "backup_incremental_data")
                # analyze commit non-incrementally and save run for comparison
                out_nonincr = os.path.join(outtry, 'non-incr')
                os.makedirs(out_nonincr)
                file_original_run = os.path.join(out_nonincr, "compare-data-nonincr")
                add_options = ['--enable', 'incremental.only-rename', '--set', 'save_run', file_original_run]
                utils.analyze_commit(analyzer_dir, gr, repo_path, build_compdb, commit.hash, out_nonincr, conf, add_options)
                # analyze commit incrementally based on the previous commit and save run for comparison
                out_incr = os.path.join(outtry, 'incr')
                os.makedirs(out_incr)
                file_incremental_run = os.path.join(out_incr, "compare-data-incr")
                add_options = ['--enable', 'incremental.load', '--enable', 'incremental.save', '--set', 'save_run', file_incremental_run]
                utils.analyze_commit(analyzer_dir, gr, repo_path, build_compdb, commit.hash, out_incr, conf, add_options)
                # compare stored data of original and incremental run
                out_compare = os.path.join(outtry, 'compare')
                os.makedirs(out_compare)
                utils.compare_runs(analyzer_dir, dummy_c_file, out_compare, conf, file_incremental_run, file_original_run)
                count_analyzed += 1
                prev_commit = commit.hash
            except utils.subprocess.CalledProcessError as e:
                print('Aborted because command ', e.cmd, 'failed.')
                shutil.rmtree("incremental_data")
                shutil.copytree("backup_incremental_data", "incremental_data")
                count_failed+=1
        analyzed_commits[try_num]=(str(commit.hash)[:6], relCLOC)

if os.path.exists(outdir) and os.path.isdir(outdir):
   shutil.rmtree(outdir)
analyze_series_in_repo()
