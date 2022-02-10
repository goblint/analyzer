#!/usr/bin/env bash
set -e # exit immediately if a command fails
set -o pipefail # or all $? in pipe instead of returning exit code of the last command only

if [ $# -lt 2 ]; then
  echo "Usage: $0 <repo_path> <start_commit> [<number of commits>]"
  exit 1
fi

repo_path=${1}
start_commit=${2}
limit=${3-"999"}
limit="$((limit+1))"
out="out"

# files to exclude from diff for a meaningful measurement of changed LOC
diff_exclude=""
# for the test runs on figlet the following files and folders were excluded
# diff_exclude=":^fonts :^tests :^chkfont.c :^getopt.c :^figfont.txt :^showfigfonts :^figmagic :^figlist :^figlet.6 :^chkfont.6 :^figlist.6
#  :^showfigfonts.6 :^Makefile :^Makefile.tc :^README :^CHANGES :^FAQ :^LICENSE :^run-tests.sh :^tests.sh :^.gitignore"

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
rm -rf "incremental_data"
function log {
  echo "$*" | tee -a $outp/incremental.log
}

mkdir -p "$outp"
log $(date)

loc=$(git -C $repo_path diff --shortstat 4b825dc642cb6eb9a060e54bf8d69288fbee4904 $start_commit -- . $diff_exclude)
git -C $repo_path checkout $start_commit
i=1
prev_commit=''
echo -e "index\tcommit\tl_ins\tl_del\tl_max\ttime\ttime(internally)\tvars\tevals\tchanged\tadded\tremoved\tchanged_start\tnew_start" >> $outp/incremental_runtime.log
while
  commit=$(git -C $repo_path rev-parse HEAD)
  if [ "$commit" = "$prev_commit" ]; then
    log "Reached last commit $commit"
    break
  fi
  if [ "$prev_commit" != '' ]; then
    loc=$(git -C $repo_path diff --shortstat $prev_commit $commit -- . $diff_exclude)
  fi
  prev_commit=$commit
  outc=$outp/$commit
  mkdir -p $outc
  git -C $repo_path show > $outc/commit.patch
  log "Analyze $i. commit $commit"
  if [ -e "$repo_path/.gob/$commit" ]; then
    log "  Incremental results for this commit already exists!"
  fi
  files=$(git -C $repo_path diff-tree --no-commit-id --name-only --root -r $commit)
  # if [ ! $(echo "$files" | grep ".*\.c$") ]; then
  if ! grep ".*\.[ch]$" > /dev/null <<< "$files"; then
    log "  No *.c or *.h files are included in this commit!"
  fi
  log "  All files: $(git -C $repo_path show --pretty=format:"" --shortstat $commit)"
  log "  *.c and *.h: $(git -C $repo_path show --pretty=format:"" --shortstat $commit -- *.c *.h)"
  start=$(echo "scale=3; $(date +%s%3N) /1000" | bc)
  # running it with (gtime -v ./goblint ...) doesn't react to ^C
  (date && ./goblint -v --conf conf/incremental.json $repo_path/Makefile 2>&1) | tee $outc/analyzer.log
  end=$(echo "scale=3; $(date +%s%3N) /1000" | bc)
  runtime=$(echo "$end-$start" | bc)
  log "  Goblint ran $runtime seconds"
  internal_runtime=$(grep 'TOTAL' $outc/analyzer.log | tr -s ' ' | cut -d" " -f2 | cut -d"s" -f1)
  vars=$(grep 'vars = ' $outc/analyzer.log | cut -d" " -f3)
  evals=$(grep 'evals = ' $outc/analyzer.log | cut -d" " -f9)
  changed=$(grep 'change_info = { ' $outc/analyzer.log | cut -d" " -f9 | cut -d";" -f1)
  added=$(grep 'change_info = { ' $outc/analyzer.log | cut -d" " -f12 | cut -d";" -f1)
  removed=$(grep 'change_info = { ' $outc/analyzer.log | cut -d" " -f15 | cut -d";" -f1)
  changed_start=$(grep 'has changed start state' $outc/analyzer.log | wc -l)
  new_start=$(grep 'New start function' $outc/analyzer.log | wc -l)
  mem_safe=$(grep -E '[^(un)]safe:' $outc/analyzer.log | tr -s ' ' | cut -d" " -f2)
  mem_vulnerable=$(grep 'vulnerable:' $outc/analyzer.log | tr -s ' ' | cut -d" " -f2)
  mem_unsafe=$(grep 'unsafe:' $outc/analyzer.log | tr -s ' ' | cut -d" " -f2)
  mem_total=$(grep 'total:' $outc/analyzer.log | tr -s ' ' | cut -d" " -f2)
  l_ins=0
  l_del=0
  if [[ $loc == *"insertion"* ]]; then
    l_ins=$(echo $loc | cut -d" " -f4)
    if [[ $loc == *"deletion"* ]]; then
      l_del=$(echo $loc | cut -d" " -f6)
    fi
  else
    if [[ $loc == *"deletion"* ]]; then
      l_del=$(echo $loc | cut -d" " -f4)
    fi
  fi
  if [[ $l_ins < $l_del ]]; then
    l_max=$l_del
  else
    l_max=$l_ins
  fi
  echo -e "$i\t$commit\t$l_ins\t$l_del\t$l_max\t$runtime\t$internal_runtime\t$vars\t$evals\t$changed\t$added\t$removed\t$changed_start\t$new_start\t$mem_safe\t$mem_vulnerable\t$mem_unsafe\t$mem_total" >> $outp/incremental_runtime.log
  log "  $(grep 'evals = ' $outc/analyzer.log)"
  log "  $(grep 'change_info = ' $outc/analyzer.log)"
  log "  Obsolete functions: $(grep 'Obsolete function' $outc/analyzer.log | wc -l)"
  log "  Changed start state: $changed_start"
  log "  New start functions: $new_start"
  i="$((i+1))"
  git_fwd # TODO use this as exit condition
  [ "$i" -lt "$limit" ]
do :; done

log $(date)
