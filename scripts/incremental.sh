#! /bin/bash
set -e # exit immediately if a command fails
set -o pipefail # or all $? in pipe instead of returning exit code of the last command only

if [ $# -lt 2 ]; then
  echo "Usage: $0 <repo_path> <start_commit> [<limit of commits>]"
  exit 1
fi

repo_path=${1}
start_commit=${2}
limit=${3-"999"}
out="out"

function git_bwd() { # checkout previous commit
  git -C $repo_path checkout HEAD^
}
function git_fwd() { # checkout next commit
  git -C $repo_path log --reverse --pretty=%H master | grep -A 1 $(git -C $repo_path rev-parse HEAD) | tail -n1 | xargs git -C $repo_path checkout
}

# function git_dirty {
#     dir=${1-"`pwd`"};
#     test -n "$(git -C $repo_path status --porcelain)"
# }
# if git_dirty "$repo_path"; then
#     echo "The repository is not in a clean state. Abort!"
#     exit 1
# fi

if [ ! -f scripts/incremental.sh ]; then
  echo "Please run from root of analyzer repo."
  exit 1
fi

if [ ! -f goblint ]; then
  echo "Binary goblint is missing!"
  exit 1
fi

function finish {
  rm -rf goblint_temp_*
}
trap finish EXIT


outp=$out/$(basename $repo_path)

rm -rf "$outp"
function log {
  echo "$*" | tee -a $outp/incremental.log
}

mkdir -p "$outp"
log $(date)

git -C $repo_path checkout $start_commit
i=1
prev_commit=''
while
  commit=$(git -C $repo_path rev-parse HEAD)
  if [ "$commit" = "$prev_commit" ]; then
    log "Reached last commit $commit"
    break
  fi
  prev_commit=$commit
  outc=$outp/$commit
  mkdir -p $outc
  git -C $repo_path show > $outc/commit.patch
  log "Analyze $i. commit $commit"
  if [ -e "$repo_path/.gob/$commit" ]; then
    log "  Incremental results for this commit already exists!"
  fi
  files=$(git -C $repo_path diff-tree --no-commit-id --name-only -r $commit)
  # if [ ! $(echo "$files" | grep ".*\.c$") ]; then
  if ! grep ".*\.[ch]$" > /dev/null <<< "$files"; then
    log "  No *.c or *.h files are included in this commit!"
  fi
  start=`date +%s`
  # running it with (gtime -v ./goblint ...) doesn't react to ^C
  (date && ./goblint -v --conf conf/incremental.json $repo_path/Makefile 2>&1) | tee $outc/analyzer.log
  end=`date +%s`
  runtime=$((end-start))
  log "  Goblint ran $runtime seconds"
  # TODO grep obsolete functions, changed start state, evals
  i="$((i+1))"
  git_fwd # TODO use this as exit condition
  [ "$i" -lt "$limit" ]
do :; done

log $(date)
