# File based on bench/scripts/incremental/benchmarking/stats.py from the Goblint bench repository
# https://github.com/goblint/bench/blob/6333415e9b5b5157b3b462b673726faa93c97488/scripts/incremental/benchmarking/stats.py

from pydriller import Repository
from datetime import datetime
from pathlib import Path
import os
import sys

if __name__ == '__main__':
  if len(sys.argv) != 2:
      print("Wrong number of parameters.\nUse script like this: python3 generate_git.py <absolute path to goblint directory>")
      exit()

analyzer_dir  = sys.argv[1]
url = 'https://github.com/facebook/zstd'
repo_name = 'zstd'
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
count_nochanges = 0
count_merge = 0
count_big = 0
count_small = 0

# Function based on bench/scripts/incremental/benchmarking/utils.py from Goblint bench repository
# https://github.com/goblint/bench/blob/6333415e9b5b5157b3b462b673726faa93c97488/scripts/incremental/benchmarking/utils.py
def _calculateRelCLOC(repo_path, commit, diff_exclude):
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

def iter_repo():
    global analyzed_commits
    global total_commits
    global count_merge
    global count_nochanges
    global count_big
    global count_small

    for commit in Repository(url, since=begin, to=to, clone_repo_to=cwd).traverse_commits():
        total_commits += 1

        # count merge commits
        if commit.merge:
            count_merge += 1
            continue

        # count commits that have less than maxCLOC of relevant code changes
        relCLOC = _calculateRelCLOC(repo_path, commit, paths_to_exclude) # use this to filter commits by actually relevant changes
        if relCLOC == 0:
            count_nochanges += 1
            continue

        if maxCLOC is not None and relCLOC > maxCLOC:
            count_big += 1
            continue

        count_small += 1

iter_repo()
print("\nCommits traversed in total: ", total_commits)
print("Merge commits: ", count_merge)
print("Commits without any relevant changes: ", count_nochanges)
print("Big commits: ", count_big)
print("Small commits with relevant changes: ", count_small)