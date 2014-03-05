#!/bin/bash
if [ "$(expr substr $(uname -s) 1 6)" != "CYGWIN" ];then
    echo This script must be run using cygwin!
    exit 1
fi
git pull
./make.sh all

# optional argument: username for server (might be different)
user=${1-`whoami`}
host=$user@www2.informatik.tu-muenchen.de
scp goblint goblint.byte $host:/srv/www/htdocs.goblint/upload_here/
ssh $host 'cd /srv/www/htdocs.goblint && sudo -u vesal ./doitall.sh'
