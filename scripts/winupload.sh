#!/bin/bash
git pull
./make.sh all
scp goblint www2.informatik.tu-muenchen.de:/srv/www/htdocs.goblint/upload_here/goblint.exe
scp goblint.byte www2.informatik.tu-muenchen.de:/srv/www/htdocs.goblint/upload_here/goblint.byte.exe
ssh www2.informatik.tu-muenchen.de 'cd /srv/www/htdocs.goblint && sudo -u vesal ./doitall.sh'
