#!/bin/sh

set -e

genspio_small_examples=_build/default/src/examples/small_examples.exe

ocaml please.mlt configure
jbuilder build @install
jbuilder build @doc
jbuilder build $genspio_small_examples

export output_path=_build/doc/html/
rm -fr $output_path
mkdir -p $output_path
cp -r _build/default/_doc/_html/* $output_path/

css_file=ssc.css
css_path=$output_path/$css_file

cat >> $output_path/odoc.css <<EOF
body {
  max-width: 72em; /* A bit larger than odoc's output */
}
EOF
cat $output_path/odoc.css > $css_path
more_css () {
    cat >> $1 <<EOF
h1 {
  border: none;
  padding: 1em;
  margin: 0px;
  font-size: 220%;
  text-align: center;
  background-color: #eee;
  border: solid 2px #400;
  padding-left: 4%;
  padding-right: 4%;
  padding-top: 2%;
  padding-bottom: 2%;
  width: 104%;
  margin-left: -2%;
  font-weight: bold;
}
h1 code {
  font-size: 100%;
  font-weight: bold;
  font-family: sans;
  background-color: #0000;
}
header {
}
#TOC {
  padding-left: 1em;
  border-left: solid 2px #400;
}
pre {
  padding-left: 1em;
}
EOF
}
more_css $css_path

call_caml2html () {
    local input_file="$1"
    local output_file="$2"
    local title="$3"
    caml2html -make-css /tmp/c2h.css
    insert_bg_color=#ede1e1
    cat > /tmp/morecode.css <<EOF
a.sourceLine { display: inline-block; line-height: 1.25; }
a.sourceLine { pointer-events: none; color: inherit; text-decoration: inherit; }
a.sourceLine:empty { height: 1.2em; position: absolute; }
.sourceCode { overflow: visible; }
code.sourceCode { white-space: pre; position: relative; }
div.sourceCode { margin: 1em 0; }
pre.sourceCode { margin: 0; }
@media screen {
div.sourceCode { overflow: auto; }
}
@media print {
code.sourceCode { white-space: pre-wrap; }
a.sourceLine { text-indent: -1em; padding-left: 1em; }
}
pre.numberSource a.sourceLine
  { position: relative; }
pre.numberSource a.sourceLine:empty
  { position: absolute; }
pre.numberSource a.sourceLine::before
  { content: attr(data-line-number);
    position: absolute; left: -5em; text-align: right; vertical-align: baseline;
    border: none; pointer-events: all;
    -webkit-touch-callout: none; -webkit-user-select: none;
    -khtml-user-select: none; -moz-user-select: none;
    -ms-user-select: none; user-select: none;
    padding: 0 4px; width: 4em;
    color: #aaaaaa;
  }
pre.numberSource { margin-left: 3em; border-left: 1px solid #aaaaaa;  padding-left: 4px; }
div.sourceCode
  {  }
@media screen {
a.sourceLine::before { text-decoration: underline; }
}
code span.al { color: #ff0000; font-weight: bold; } /* Alert */
code span.an { color: #60a0b0; font-weight: bold; font-style: italic; } /* Annotation */
code span.at { color: #7d9029; } /* Attribute */
code span.bn { color: #40a070; } /* BaseN */
code span.bu { } /* BuiltIn */
code span.cf { color: #007020; font-weight: bold; } /* ControlFlow */
code span.ch { color: #4070a0; } /* Char */
code span.cn { color: #880000; } /* Constant */
code span.co { color: #60a0b0; font-style: italic; } /* Comment */
code span.cv { color: #60a0b0; font-weight: bold; font-style: italic; } /* CommentVar */
code span.do { color: #ba2121; font-style: italic; } /* Documentation */
code span.dt { color: #902000; } /* DataType */
code span.dv { color: #40a070; } /* DecVal */
code span.er { color: #ff0000; font-weight: bold; } /* Error */
code span.ex { } /* Extension */
code span.fl { color: #40a070; } /* Float */
code span.fu { color: #06287e; } /* Function */
code span.im { } /* Import */
code span.in { color: #60a0b0; font-weight: bold; font-style: italic; } /* Information */
code span.kw { color: #007020; font-weight: bold; } /* Keyword */
code span.op { color: #666666; } /* Operator */
code span.ot { color: #007020; } /* Other */
code span.pp { color: #bc7a00; } /* Preprocessor */
code span.sc { color: #4070a0; } /* SpecialChar */
code span.ss { color: #bb6688; } /* SpecialString */
code span.st { color: #4070a0; } /* String */
code span.va { color: #19177c; } /* Variable */
code span.vs { color: #4070a0; } /* VerbatimString */
code span.wa { color: #60a0b0; font-weight: bold; font-style: italic; } /* Warning */
.insert {background-color: $insert_bg_color ;
         padding-left: 6px; padding-top: 1em; padding-bottom: 1em }
.insert pre { background-color: $insert_bg_color ;
      margin-left: 3em ;
      margin-right: 3em;
      border-left: 1px solid #aaaaaa; }
.insert code { background-color: $insert_bg_color }
EOF
    cat $css_path > $output_path/caml2html.css
    cat /tmp/c2h.css >> $output_path/caml2html.css
    cat /tmp/morecode.css >> $output_path/caml2html.css
    more_css $output_path/caml2html.css
    caml2html $input_file -charset UTF-8 \
              -t -cssurl caml2html.css \
              -ext md:'cat | { printf "<div class=insert>" ; pandoc -w html ; printf "</div>" ; } ' \
              -o $output_file
    sed -i "s:${input_file}:${title}:" $output_file
    sed -i 's@<em>This document was generated using@<em>Back to <a href="index.html">home</a>.</em>@' $output_file
    sed -i 's@<a href="http://martin.jambon.free.fr/caml2html.html">caml2html</a></em>@@' $output_file
    echo "Made $output_file"
}

call_caml2html src/lib/to_slow_flow.ml $output_path/to-slow-flow.html \
               "The Slow-flow Compiler"
call_caml2html src/lib/transform.ml $output_path/transform-module.html \
               "The AST Transformations"
call_caml2html src/examples/service_composer.ml $output_path/service-composer-example.html \
               "The Service Composer Example"


pandocify () {
    local title="$(head -n 1 $1)"
    echo "$1 -> $2 -> $title"
    tail -n +3 $1 \
        | sed 's:(./doc/\(.*\)\.md):(\1.html):g' \
        | sed 's:(./\([^/]*\)\.md):(\1.html):g' \
        | sed 's:\(`Genspio.\([^`]*\)`\):[\1](genspio/Genspio/\2/index.html):g' \
        | sed 's:usage examples:[usage examples](./small-examples.html):' \
        | sed 's:<!--TOSLOWFLOW-->:- Code [documentation](./to-slow-flow.html) for the `To_slow_flow` *compiler.*:' \
        | sed 's:<!--TRANSFORM-->:- Code [documentation](./transform-module.html) for the `Transform` module (AST *optimizations*).:' \
        | sed 's:<!--SERCOEX-->:- Code [documentation](./service-composer-example.html) for the *“Service-composer Example”*.:' \
        | pandoc -c $css_file -s \
                 --variable title="$title" --variable pagetitle="$title" \
                 --toc -o $output_path/$2.html
}
pandocify README.md index
for f in $(find doc -type f -name '*.md') ; do
    pandocify $f $(basename ${f%.md})
done

$genspio_small_examples > /tmp/examples.md
pandocify /tmp/examples.md small-examples

echo "Done cf. file://$PWD/$output_path/index.html"
