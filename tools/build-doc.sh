#!/bin/sh

set -e

ocaml please.ml configure
jbuilder build @install
jbuilder build @doc

rm -fr _build/doc/html/
mkdir -p _build/doc/html/
cp -r _build/default/_doc/* _build/doc/html/

pandocify () {
    sed 's:(./doc/\(.*\)\.md):(\1.html):g' $1 \
        | sed 's:(./\([^/]*\)\.md):(\1.html):g' \
        | sed 's:\(`Genspio.\([^`]*\)`\):[\1](genspio/Genspio/\2/index.html):g' \
        | pandoc -c odoc.css -s -o _build/doc/html/$2.html
}
pandocify README.md index
for f in $(find doc -type f -name '*.md') ; do
    pandocify $f $(basename ${f%.md})
done
    

echo Done
