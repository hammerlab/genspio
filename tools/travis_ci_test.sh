#!/usr/bin/env bash



travis_install_on_linux () {
    # Install OCaml and OPAM PPAs
    export ppa=avsm/ocaml42+opam12

    echo "yes" | sudo add-apt-repository ppa:$ppa
    sudo apt-get update -qq

    export opam_init_options="--comp=$OCAML_VERSION"
    sudo apt-get install -qq  opam time git

    export important_shells=bash,dash,busybox
}

travis_install_on_osx () {
    curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
    sudo hdiutil attach XQuartz-2.7.6.dmg
    sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /

    brew update
    brew install opam
    export opam_init_options="--comp=$OCAML_VERSION"

    # Get a POSIX shell
    brew install dash

    # the tests require more than the default limit
    ulimit -n 2048

    export important_shells=dash
}


case $TRAVIS_OS_NAME in
  osx) travis_install_on_osx ;;
  linux) travis_install_on_linux ;;
  *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
esac

# configure and view settings
export OPAMYES=1
echo "ocaml -version"
ocaml -version
echo "opam --version"
opam --version
echo "git --version"
git --version

# install OCaml packages
opam init $opam_init_options
eval `opam config env`

opam update

opam pin add genspio .
opam install genspio

export OCAMLPARAM='warn-error=Ad-58,_'

genspio_test=_build/default/src/test/main.exe
genspio_downloader_maker=_build/default/src/examples/downloader.exe

echo "================== BUILD ALL ==================================================="
ocaml please.ml configure
jbuilder build @install
jbuilder build $genspio_test
jbuilder build $genspio_downloader_maker

echo "================== TESTS ======================================================="

$genspio_test --important-shells $important_shells _test/
(
    cd _test
    case $TRAVIS_OS_NAME in
        osx) # On OSX we do less tests because they take too long on Travis
            (
                cd dash-Std1L
                make
                make check
            ) ;;
        linux)
            make run-all
            make check
            ;;
        *) echo "Unknown $TRAVIS_OS_NAME"; exit 1
    esac
)


echo "================== EXAMPLES: TEST 1 ============================================"
genspio_downloader=/tmp/genspio-downloader
$genspio_downloader_maker make $genspio_downloader

dash $genspio_downloader -h

dash $genspio_downloader -c -t /tmp/test1 -f k3.0.0.tar.gz -u https://github.com/hammerlab/ketrew/archive/ketrew.3.0.0.tar.gz
ls -la /tmp/test1
test -f /tmp/test1/k3.0.0.tar
test -f /tmp/test1/ketrew-ketrew.3.0.0/README.md


echo "================== EXAMPLES: TEST 2 ============================================"
dash $genspio_downloader -c -t /tmp/genstest2 -u https://www.dropbox.com/s/h16b8ak9smkgw3g/test.tar.gz.zip.bz2.tbz2?raw=1
ls -la /tmp/genstest2
test -f /tmp/genstest2/src/lib/EDSL.ml

echo "================== EXAMPLES: TEST 3 ============================================"
# like -t /tmp/test2, without -c (which is fragile w.r.t. tar)
mkdir -p /tmp/test3
cd /tmp/test3
dash $genspio_downloader -u https://github.com/hammerlab/ketrew/archive/ketrew.3.0.0.tar.gz
ls -la /tmp/test3
test -f /tmp/test3/ketrew-ketrew.3.0.0/README.md




