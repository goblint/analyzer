import utils
from pydriller import Repository
from datetime import datetime
import os
import sys

if __name__ == '__main__':
  if len(sys.argv) != 2:
      print("Wrong number of parameters.\nUse script like this: python3 incremental_stats.py <path to goblint directory>")
      exit()

analyzer_dir  = sys.argv[1]
url = 'https://github.com/facebook/zstd'
repo_name = 'zstd'
conf = "big-benchmarks1"
build_script = 'build_compdb_zstd.sh'
begin = datetime(2021,8,1)
to = datetime(2022,2,1)
maxCLOC       = 50
dirs_to_exclude  = ["build", "doc", "examples", "tests", "zlibWrapper", "contrib"]

cwd  = os.getcwd()
outdir = os.path.join(cwd, 'out')
repo_path = os.path.normpath(os.path.join(cwd, repo_name))
paths_to_exclude = list(map(lambda x: os.path.join(repo_path, x), dirs_to_exclude))

analyzed_commits = {}
total_commits = 0
count_merge = 0
count_big = 0
count_small = 0

def iter_repo():
    global analyzed_commits
    global total_commits
    global count_big
    global count_merge
    global count_small

    for commit in Repository(url, since=begin, to=to, clone_repo_to=cwd).traverse_commits():
        total_commits += 1

        # count merge commits
        if commit.merge:
            count_merge += 1
            continue

        # count commits that have less than maxCLOC of relevant code changes
        relCLOC = utils.calculateRelCLOC(repo_path, commit, paths_to_exclude) # use this to filter commits by actually relevant changes
        if maxCLOC is not None and relCLOC > maxCLOC:
            count_big += 1
            continue

        count_small += 1

iter_repo()
print("\nCommits traversed in total: ", total_commits)
print("Merge commits: ", count_merge)
print("Big commits: ", count_big)
print("Small commits: ", count_small)
