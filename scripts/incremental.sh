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
git -C $repo_path checkout $start_commit
i=1
while
  commit=$(git -C $repo_path rev-parse HEAD)
  outc=$outp/$commit
  mkdir -p $outc
  echo "Analyze $i. commit $commit" | tee -a $outp/incremental.log
  git -C $repo_path show > $outc/commit.patch
  start=`date +%s`
  time ./goblint -v --conf conf/incremental.json $repo_path/Makefile | tee $outc/analyzer.log
  end=`date +%s`
  runtime=$((end-start))
  echo "Done after $runtime seconds" | tee -a $outp/incremental.log
  i="$((i+1))"
  git_fwd # TODO use this as exit condition
  [ "$i" -lt "$limit" ]
do :; done
