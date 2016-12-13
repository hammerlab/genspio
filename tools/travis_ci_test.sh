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

    export important_shells=bash,dash
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

opam install ppx_deriving
opam install pvem_lwt_unix

export OCAMLPARAM='warn-error=Ad-58,_'
export WITH_TESTS=true
make byte
make native

gennspio_test=_build/src/test/genspio-test.byte
genspio_examples=_build/src/test/genspio-examples.byte

$genspio_test

$genspio_examples dl -
