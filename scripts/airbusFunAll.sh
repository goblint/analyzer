set -o errexit
set -o pipefail
if [ $# -eq 0 ]; then
    echo "Usage: $0 <git log options>"
    echo "e.g. --since=2weeks or -n 3 for limiting to the last 3 commits. See git --help log"
    exit 1
fi
analyzer=~/analyzer
if [ `pwd` = $analyzer ]; then
    echo "Do not run in $analyzer! Go to some working dir. Everything will be copied."
    exit 1
fi
# cp $analyzer/scripts/airbusFun.sh .
pushd $analyzer
branch=`git rev-parse --abbrev-ref HEAD`
if [ "$branch" = "HEAD" ]; then
    echo "Currently in detached head state. Checkout a branch first!"
    exit 1
fi
echo "Current branch is $branch"
commits=$(git log $* --reverse --pretty=format:"%h")
if test -z "$commits"; then
    echo "Found no commits! Try different search options..."
    exit 1
fi
echo "Will checkout the following commits in that order:"
echo
git log $* --reverse
echo
read -p "Press [Enter] to continue"
echo
for sha in $commits; do
    echo "Checkout $sha"
    git checkout -q $sha
    popd
    ~/Dropbox/airbus/airbusFun.sh success init
    ~/Dropbox/airbus/airbusFun.sh fail init
    pushd $analyzer
done
echo "Returning to newest commit from $branch"
git checkout -q $branch
popd
