#! /bin/sh

set -e

tmpdir=/tmp/multigittest/

rm -fr $tmpdir

try_cmd () {
    echo "================================================================================"
    echo "==== Running: [ $1 ]"
    sh -c "$1"
}

try_cmd 'git multi-status -h'

try_cmd "git multi-status --version"

moregits=$tmpdir/moregits
mkdir -p $moregits
(
    cd $moregits
    git clone https://github.com/hammerlab/ketrew.git
    git clone https://github.com/hammerlab/biokepi.git
    echo "GREEEAAAT" >> biokepi/README.md
    echo "Boooo" >> biokepi/LICENSE
    git clone https://github.com/hammerlab/coclobas.git
    echo "GREEEAAAT" >> coclobas/README.md
    echo "Stuff" > coclobas/doeas-not-exist
    git clone https://gitlab.com/smondet/genspio-doc.git
)

gms="git multi-status --no-config"

try_cmd "$gms $moregits"
try_cmd "$gms $moregits --show-modified"

try_cmd "$gms $moregits 2>&1 | grep ketrew   | egrep 'M: *0'"
try_cmd "$gms $moregits 2>&1 | grep biokepi  | egrep 'M: *2'"
try_cmd "$gms $moregits 2>&1 | grep coclobas | egrep 'U: *1'"

try_cmd "$gms $moregits 2>&1 --show-modified  | grep 'README.md'"
try_cmd "$gms $moregits 2>&1 --show-modified  | grep 'LICENSE'"


try_cmd "$gms $moregits 2>&1 | grep 'GHub: coclobas'"
try_cmd "$gms $moregits 2>&1 | grep 'GLab: genspio-doc'"

gar="git activity-report --no-config"

try_cmd "$gar $moregits --version"

try_cmd "$gar $moregits"

try_cmd "$gar $moregits --since 2018-07-01"

try_cmd "$gar $moregits  --since 2018-08-01 --section-base '###'"

