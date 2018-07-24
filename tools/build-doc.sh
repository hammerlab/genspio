#!/bin/sh

set -e

genspio_small_examples=_build/default/src/examples/small_examples.exe

ocaml please.mlt configure
jbuilder build @install
jbuilder build @doc
jbuilder build $genspio_small_examples

rm -fr _build/doc/html/
mkdir -p _build/doc/html/
cp -r _build/default/_doc/_html/* _build/doc/html/

pandocify () {
    local title="$(head -n 1 $1)"
    echo "$1 -> $2 -> $title"
    tail -n +3 $1 \
        | sed 's:(./doc/\(.*\)\.md):(\1.html):g' \
        | sed 's:(./\([^/]*\)\.md):(\1.html):g' \
        | sed 's:\(`Genspio.\([^`]*\)`\):[\1](genspio/Genspio/\2/index.html):g' \
        | sed 's:usage examples:[usage examples](./small-examples.html):' \
        | pandoc -c odoc.css -s \
                 --variable title="$title" --variable pagetitle="$title" \
                 --toc -o _build/doc/html/$2.html
}
pandocify README.md index
for f in $(find doc -type f -name '*.md') ; do
    pandocify $f $(basename ${f%.md})
done

$genspio_small_examples > /tmp/examples.md
pandocify /tmp/examples.md small-examples

echo Done
