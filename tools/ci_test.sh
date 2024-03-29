#!/usr/bin/env bash

# configure and view settings
export OPAMYES=1
#echo "ocaml -version"
#ocaml -version
echo "opam --version"
opam --version
echo "git --version"
git --version

echo "Which bash"
which bash
echo "bash --version"
bash --version
echo "/bin/bash --version"
/bin/bash --version || echo "NO /bin/bash version"
echo "/bin/sh --version"
/bin/sh --version || echo "NO /bin/sh version"
echo ">>> getconf ARG_MAX:"
getconf ARG_MAX
echo ">>> getconf -a:"
getconf -a || echo "Not on OSX...?"
echo ">>> xargs shows the limits:"
echo "$(xargs --show-limits & { sleep 1 ; exit 0 ; } )"
echo ">>> ULimits:"
ulimit -a

export important_shells=bash
export main_shell=bash

genspio_test=src/test/main.exe
genspio_downloader_maker=src/examples/downloader.exe
genspio_small_examples=src/examples/small_examples.exe
genspio_vm_tester=src/examples/vm_tester.exe
genspio_service_composer=src/examples/service_composer.exe
genspio_multigit=src/examples/multigit.exe

echo "================== BUILD ALL ==================================================="
dune build @install

dune build $genspio_test
dune build $genspio_downloader_maker
dune build $genspio_small_examples
dune build $genspio_vm_tester
dune build $genspio_service_composer
dune build $genspio_multigit

echo "================== TESTS ======================================================="

dune exec $genspio_test -- --run-constant-propagation-tests \
     --important-shells $important_shells _test/
(
    cd _test
##    case $TRAVIS_OS_NAME in
##        osx)
##            (
##                echo "On OSX we do less tests because they take too long on Travis"
##                cd $main_shell-StdML
##                echo "Make $main_shell-StdML"
##                make
##                echo "Make Check"
##                make check
##                cd ../sh-SlowFlow/
##                echo "Make sh-SlowFlow"
##                make
##                echo "Make Check"
##                make check
##            ) ;;
##        linux)
            make run-all
            make check
##            ;;
##        *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
##    esac
)


echo "================== EXAMPLES: TEST 1 ============================================"
genspio_downloader=/tmp/genspio-downloader
dune exec $genspio_downloader_maker -- make $genspio_downloader

$main_shell $genspio_downloader -h

$main_shell $genspio_downloader -c -t /tmp/test1 -f k3.0.0.tar.gz -u https://github.com/hammerlab/ketrew/archive/ketrew.3.0.0.tar.gz
ls -la /tmp/test1
test -f /tmp/test1/k3.0.0.tar
test -f /tmp/test1/ketrew-ketrew.3.0.0/README.md


echo "================== EXAMPLES: TEST 2 ============================================"
$main_shell $genspio_downloader -c -t /tmp/genstest2 -u https://www.dropbox.com/s/h16b8ak9smkgw3g/test.tar.gz.zip.bz2.tbz2?raw=1
ls -la /tmp/genstest2
test -f /tmp/genstest2/src/lib/EDSL.ml

echo "================== EXAMPLES: TEST 3 ============================================"
# like -t /tmp/test2, without -c (which is fragile w.r.t. tar)
(
    mkdir -p /tmp/test3
    cd /tmp/test3
    $main_shell $genspio_downloader -u https://github.com/hammerlab/ketrew/archive/ketrew.3.0.0.tar.gz
    ls -la /tmp/test3
    test -f /tmp/test3/ketrew-ketrew.3.0.0/README.md
)

echo "================== EXAMPLES: SMALL ONES ============================================"

dune exec $genspio_small_examples

echo "================== EXAMPLES: vm_tester ============================================"

dune exec $genspio_vm_tester -- --vm arm-owrt /tmp/vmt/arm-owrt/  ; ( cd /tmp/vmt/arm-owrt/ ; make help ; )
dune exec $genspio_vm_tester -- --vm arm-dw /tmp/vmt/arm-dw/      ; ( cd /tmp/vmt/arm-dw/ ; make help ; )
dune exec $genspio_vm_tester -- --vm amd64-fb /tmp/vmt/amd64-fb/  ; ( cd /tmp/vmt/amd64-fb ; make help ; )


echo "================== EXAMPLES: Service-composer======================================="

dune exec $genspio_service_composer -- --name cosc --output-path $HOME/bin

echo "================== EXAMPLES: Multigit ======================================="

dune exec $genspio_multigit -- $HOME/bin
export PATH=$HOME/bin:$PATH
./tools/multigit-test.sh


##echo "================== Trigger Docker build ======================================"
##
##git branch --all
##
##echo "TRAVIS_BRANCH: $TRAVIS_BRANCH"
##echo "DOCKER_BUILD: $DOCKER_BUILD"
##
##if [ "$TRAVIS_BRANCH" = "master" ] && [ "$DOCKER_BUILD" = "true" ] ; then
##    curl -H "Content-Type: application/json" \
##         --data '{"source_type": "Branch", "source_name": "apps406"}' \
##         -X POST \
##         https://registry.hub.docker.com/u/smondet/genspio-doc-dockerfiles/trigger/f113ff75-c7d4-446d-9a71-2e4d7db63389/
##else
##    echo "Not triggering the Docker build this time."
##fi


